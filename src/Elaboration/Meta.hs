{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Elaboration.Meta where

import qualified Core.Syntax as Syntax
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import Data.EnumSet (EnumSet)
import qualified Data.EnumSet as EnumSet
import Data.List (partition)
import Data.Persist
import qualified Meta
import Orphans ()
import qualified Postponement
import Protolude hiding (State, link, link2, state)
import qualified Span
import Telescope (Telescope)
import qualified Telescope

data Entry m
  = Unsolved (Syntax.Type Void) !Int (EnumSet Postponement.Index) !Span.Relative
  | Solved (Syntax.Term Void) !CachedMetas (Syntax.Type Void)
  | LazilySolved !(m (Syntax.Term Void)) (Syntax.Type Void)

data CachedMetas = CachedMetas
  { direct :: EnumSet Meta.Index
  , unsolved :: EnumSet Meta.Index -- Or unknown
  , solved :: EnumSet Meta.Index
  }
  deriving (Eq, Show, Generic, Persist, Hashable)

instance Semigroup CachedMetas where
  CachedMetas a1 b1 c1 <> CachedMetas a2 b2 c2 =
    CachedMetas (a1 <> a2) (b1 <> b2) (c1 <> c2)

instance Monoid CachedMetas where
  mempty = CachedMetas mempty mempty mempty

entryType :: Entry m -> Syntax.Type Void
entryType entry =
  case entry of
    Unsolved type_ _ _ _ ->
      type_
    Solved _ _ type_ ->
      type_
    LazilySolved _ type_ ->
      type_

data State m = State
  { entries :: EnumMap Meta.Index (Entry m)
  , nextIndex :: !Meta.Index
  }

empty :: State m
empty =
  State
    { entries = mempty
    , nextIndex = Meta.Index 0
    }

lookup :: Meta.Index -> State m -> Entry m
lookup index state =
  entries state EnumMap.! index

new :: Syntax.Term Void -> Int -> Span.Relative -> State m -> (State m, Meta.Index)
new type_ arity span state =
  let index =
        nextIndex state
   in ( State
          { entries = EnumMap.insert index (Unsolved type_ arity mempty span) $ entries state
          , nextIndex = nextIndex state + 1
          }
      , index
      )

solve :: Meta.Index -> Syntax.Term Void -> State m -> (State m, (Int, EnumSet Postponement.Index))
solve index term state =
  (state {entries = entries'}, data_)
  where
    (data_, entries') =
      EnumMap.alterF alter index $ entries state

    alter maybeVar =
      case maybeVar of
        Nothing ->
          panic "Solving non-existent meta variable"
        Just (Unsolved type_ arity' postponed' _) -> do
          let metas =
                termMetas term <> termMetas type_
          ((arity', postponed'), Just $ Solved term mempty {direct = metas, unsolved = metas} type_)
        Just Solved {} ->
          panic "Solving an already solved meta variable"
        Just LazilySolved {} ->
          panic "Solving an already solved meta variable"

lazilySolve :: Meta.Index -> m (Syntax.Term Void) -> State m -> (State m, (Int, EnumSet Postponement.Index))
lazilySolve index mterm state =
  (state {entries = entries'}, data_)
  where
    (data_, entries') =
      EnumMap.alterF alter index $ entries state

    alter maybeVar =
      case maybeVar of
        Nothing ->
          panic "Solving non-existent meta variable"
        Just (Unsolved type_ arity' postponed' _) ->
          ((arity', postponed'), Just $ LazilySolved mterm type_)
        Just Solved {} ->
          panic "Solving an already solved meta variable"
        Just LazilySolved {} ->
          panic "Solving an already solved meta variable"

addPostponedIndex :: Meta.Index -> Postponement.Index -> State m -> State m
addPostponedIndex index postponementIndex state =
  state {entries = EnumMap.adjust adjust index $ entries state}
  where
    adjust entry =
      case entry of
        Unsolved type_ arity postponed span ->
          Unsolved type_ arity (EnumSet.insert postponementIndex postponed) span
        Solved {} ->
          panic "Adding postponement index to an already solved meta variable"
        LazilySolved {} ->
          panic "Adding postponement index to an already solved meta variable"

addPostponedIndices :: Meta.Index -> EnumSet Postponement.Index -> State m -> State m
addPostponedIndices index postponementIndices state =
  state {entries = EnumMap.adjust adjust index $ entries state}
  where
    adjust entry =
      case entry of
        Unsolved type_ arity postponed span ->
          Unsolved type_ arity (postponementIndices <> postponed) span
        Solved {} ->
          panic "Adding postponement index to an already solved meta variable"
        LazilySolved {} ->
          panic "Adding postponement index to an already solved meta variable"

-------------------------------------------------------------------------------
-- Eager entries

data EagerEntry
  = EagerUnsolved (Syntax.Type Void) !Int (EnumSet Postponement.Index) !Span.Relative
  | EagerSolved (Syntax.Term Void) CachedMetas (Syntax.Type Void)
  deriving (Eq, Generic, Persist, Hashable)

data EagerState = EagerState
  { eagerEntries :: EnumMap Meta.Index EagerEntry
  , eagerNextIndex :: !Meta.Index
  }
  deriving (Eq, Generic, Persist, Hashable)

fromEagerState :: EagerState -> State m
fromEagerState state =
  State
    { entries = fromEagerEntry <$> eagerEntries state
    , nextIndex = eagerNextIndex state
    }

fromEagerEntry :: EagerEntry -> Entry m
fromEagerEntry entry =
  case entry of
    EagerUnsolved type_ arity postponements span ->
      Unsolved type_ arity postponements span
    EagerSolved term metas type_ ->
      Solved term metas type_

toEagerEntry :: Monad m => Entry m -> m EagerEntry
toEagerEntry entry =
  case entry of
    Unsolved type_ arity postponements span ->
      pure $ EagerUnsolved type_ arity postponements span
    Solved solution metas type_ ->
      pure $ EagerSolved solution metas type_
    LazilySolved msolution type_ -> do
      solution <- msolution
      let metas =
            termMetas solution <> termMetas type_
      pure $ EagerSolved solution mempty {direct = metas, unsolved = metas} type_

toEagerState :: Monad m => State m -> Syntax.Definition -> Maybe (Syntax.Type Void) -> m EagerState
toEagerState state definition maybeType = do
  entries_ <- go (definitionMetas definition <> foldMap termMetas maybeType) mempty
  pure
    EagerState
      { eagerEntries = entries_
      , eagerNextIndex = nextIndex state
      }
  where
    go todo done
      | EnumMap.null todo' =
        pure done
      | otherwise = do
        newlyDone <- traverse toEagerEntry todo'

        let newTodo =
              foldMap
                ( \case
                    EagerUnsolved type_ _arity _postponements _span ->
                      termMetas type_
                    EagerSolved _solution metas _type ->
                      direct metas
                )
                newlyDone

        go newTodo (done <> newlyDone)
      where
        todo' =
          EnumMap.difference (EnumMap.fromSet (entries state EnumMap.!) todo) done

-------------------------------------------------------------------------------

solutionMetas :: Monad m => Meta.Index -> State m -> m (Maybe CachedMetas, State m)
solutionMetas metaIndex state = do
  case lookup metaIndex state of
    Unsolved {} ->
      pure (Nothing, state)
    Solved solution metas type_
      | EnumSet.null $ unsolved metas ->
        pure (Just metas, state)
      | otherwise ->
        flip runStateT state $ do
          indirects <- forM (EnumSet.toList $ unsolved metas) $ \i ->
            (,) i <$> StateT (solutionMetas i)

          let (directUnsolvedMetas, directSolvedMetas) =
                bimap (EnumSet.fromList . map fst) (EnumSet.fromList . map fst) $
                  partition (isNothing . snd) indirects

              indirectMetas =
                foldMap (fold . snd) indirects

              solved' =
                solved metas <> directSolvedMetas <> solved indirectMetas

              unsolved' =
                directUnsolvedMetas <> unsolved indirectMetas

              metas' =
                metas
                  { unsolved = unsolved'
                  , solved = solved'
                  }

          modify $ \s -> s {entries = EnumMap.insert metaIndex (Solved solution metas' type_) $ entries s}

          pure $ Just metas'
    LazilySolved msolution type_ -> do
      solution <- msolution
      let metas =
            termMetas solution <> termMetas type_
      solutionMetas
        metaIndex
        state
          { entries =
              EnumMap.insert metaIndex (Solved solution CachedMetas {direct = metas, unsolved = metas, solved = mempty} type_) $ entries state
          }

definitionMetas :: Syntax.Definition -> EnumSet Meta.Index
definitionMetas definition =
  case definition of
    Syntax.TypeDeclaration type_ ->
      termMetas type_
    Syntax.ConstantDefinition term ->
      termMetas term
    Syntax.DataDefinition _boxity tele ->
      dataDefinitionMetas tele

dataDefinitionMetas :: Telescope binding Syntax.Type Syntax.ConstructorDefinitions v -> EnumSet Meta.Index
dataDefinitionMetas tele =
  case tele of
    Telescope.Empty (Syntax.ConstructorDefinitions constructorDefinitions) ->
      foldMap termMetas constructorDefinitions
    Telescope.Extend _binding type_ _plixity tele' ->
      termMetas type_ <> dataDefinitionMetas tele'

termMetas :: Syntax.Term v -> EnumSet Meta.Index
termMetas term =
  case term of
    Syntax.Var _ ->
      mempty
    Syntax.Global _ ->
      mempty
    Syntax.Con _ ->
      mempty
    Syntax.Lit _ ->
      mempty
    Syntax.Meta index ->
      EnumSet.singleton index
    Syntax.PostponedCheck {} ->
      panic "Elaboration.Meta.termMetas: PostponedCheck"
    Syntax.Lets lets ->
      letsMetas lets
    Syntax.Pi _binding domain _plicity target ->
      termMetas domain <> termMetas target
    Syntax.Fun domain _plicity target ->
      termMetas domain <> termMetas target
    Syntax.Lam _bindings type_ _plicity body ->
      termMetas type_ <> termMetas body
    Syntax.App function _plicity argument ->
      termMetas function <> termMetas argument
    Syntax.Case scrutinee branches maybeDefaultBranch ->
      termMetas scrutinee
        <> case branches of
          Syntax.LiteralBranches literalBranches ->
            foldMap (foldMap termMetas) literalBranches
          Syntax.ConstructorBranches _constructorTypeName constructorBranches ->
            foldMap (foldMap telescopeMetas) constructorBranches
        <> foldMap termMetas maybeDefaultBranch
    Syntax.Spanned _span term' ->
      termMetas term'

telescopeMetas :: Telescope binding Syntax.Type Syntax.Term v -> EnumSet Meta.Index
telescopeMetas tele =
  case tele of
    Telescope.Empty term ->
      termMetas term
    Telescope.Extend _binding type_ _plixity tele' ->
      termMetas type_ <> telescopeMetas tele'

letsMetas :: Syntax.Lets v -> EnumSet Meta.Index
letsMetas lets =
  case lets of
    Syntax.LetType _ type_ lets' ->
      termMetas type_ <> letsMetas lets'
    Syntax.Let _ _ term lets' ->
      termMetas term <> letsMetas lets'
    Syntax.In term ->
      termMetas term
