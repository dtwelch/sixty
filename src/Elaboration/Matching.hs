{-# language OverloadedStrings #-}
{-# language RankNTypes #-}
{-# language ViewPatterns #-}
module Elaboration.Matching where

import Protolude hiding (IntMap, IntSet, force, try)

import {-# source #-} qualified Elaboration
import qualified Builtin
import Control.Exception.Lifted
import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Core.Binding (Binding)
import qualified Core.Binding as Binding
import Core.Bindings (Bindings)
import qualified Core.Bindings as Bindings
import qualified Core.Domain as Domain
import Core.Domain.Pattern (Pattern)
import qualified Core.Domain.Pattern as Pattern
import qualified Core.Domain.Telescope as Domain (Telescope)
import qualified Core.Domain.Telescope as Domain.Telescope
import qualified Core.Evaluation as Evaluation
import qualified Core.Readback as Readback
import qualified Core.Syntax as Syntax
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified Data.IntMap as IntMap
import qualified Data.IntSeq as IntSeq
import Data.IORef.Lifted
import Data.OrderedHashMap (OrderedHashMap)
import qualified Data.OrderedHashMap as OrderedHashMap
import qualified Data.Set as Set
import Data.Tsil (Tsil)
import qualified Data.Tsil as Tsil
import Elaboration.Context (Context)
import qualified Elaboration.Context as Context
import qualified Elaboration.Matching.SuggestedName as SuggestedName
import qualified Elaboration.Unification as Unification
import qualified Elaboration.Unification.Indices as Indices
import qualified Environment
import qualified Error
import qualified Flexibility
import GHC.Exts (fromList)
import Literal (Literal)
import qualified Meta
import Monad
import Name (Name(Name))
import qualified Name
import Plicity
import qualified Query
import Rock
import qualified Scope
import qualified Span
import qualified Surface.Syntax as Surface
import Telescope (Telescope)
import qualified Telescope
import Var

data Config = Config
  { _expectedType :: !Domain.Value
  , _scrutinees :: ![(Plicity, Domain.Value)]
  , _clauses :: [Clause]
  , _usedClauses :: !(IORef (Set Span.Relative))
  , _matchKind :: !Error.MatchKind
  }

data Clause = Clause
  { _span :: !Span.Relative
  , _matches :: [Match]
  , _rhs :: !Surface.Term
  }

data Match = Match !Domain.Value !Domain.Value !Plicity !Surface.Pattern !Domain.Type

-------------------------------------------------------------------------------

elaborateCase
  :: Context v
  -> Syntax.Term v
  -> Domain.Type
  -> [(Surface.Pattern, Surface.Term)]
  -> Domain.Type
  -> Context.CanPostpone
  -> M (Syntax.Term v)
elaborateCase context scrutinee scrutineeType branches expectedType canPostpone = do
  scrutineeValue <- Elaboration.evaluate context scrutinee
  blockingMetaOrIsPatternScrutinee <- runExceptT $ isPatternValue context scrutineeValue
  case (canPostpone, blockingMetaOrIsPatternScrutinee) of
    (Context.CanPostpone, Left blockingMeta) ->
      postponeElaborateCase context scrutinee scrutineeType branches expectedType blockingMeta

    _ -> do
      (context', var) <-
        if fromRight False blockingMetaOrIsPatternScrutinee then
          Context.extendDef context "scrutinee" scrutineeValue scrutineeType

        else
          Context.extend context "scrutinee" scrutineeType

      let
        scrutineeVarValue =
          Domain.var var
      usedClauses <- newIORef mempty
      term <- elaborateWithCoverage context' Config
        { _expectedType = expectedType
        , _scrutinees = pure (Explicit, scrutineeVarValue)
        , _clauses =
          [ Clause
            { _span = Span.add patSpan rhsSpan
            , _matches = [Match scrutineeVarValue scrutineeVarValue Explicit pat scrutineeType]
            , _rhs = rhs'
            }
          | (pat@(Surface.Pattern patSpan _), rhs'@(Surface.Term rhsSpan _)) <- branches
          ]
        , _usedClauses = usedClauses
        , _matchKind = Error.Branch
        }
      scrutineeType' <- Readback.readback (Context.toEnvironment context) scrutineeType
      pure $ Syntax.Let "scrutinee" scrutinee scrutineeType' term

postponeElaborateCase
  :: Context v
  -> Syntax.Term v
  -> Domain.Type
  -> [(Surface.Pattern, Surface.Term)]
  -> Domain.Type
  -> Meta.Index
  -> M (Syntax.Term v)
postponeElaborateCase context scrutinee scrutineeType branches expectedType blockingMeta = do
  resultMetaValue <- Context.newMeta context expectedType
  resultMetaTerm <- Elaboration.readback context resultMetaValue
  postponementIndex <- Context.newPostponedCheck context blockingMeta $ \canPostpone -> do
    resultTerm <- elaborateCase context scrutinee scrutineeType branches expectedType canPostpone
    resultValue <- Elaboration.evaluate context resultTerm
    success <- Context.try_ context $ Unification.unify context Flexibility.Rigid resultValue resultMetaValue
    if success then
      pure resultTerm
    else
      Elaboration.readback context $
        Domain.Neutral (Domain.Global Builtin.fail) $ Domain.Apps $ pure (Explicit, expectedType)
  pure $ Syntax.PostponedCheck postponementIndex resultMetaTerm

isPatternValue :: Context v -> Domain.Value -> ExceptT Meta.Index M Bool
isPatternValue context value = do
  value' <- lift $ Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var _) Domain.Empty ->
      pure True

    Domain.Neutral (Domain.Var _) (_ Domain.:> _) ->
      pure False

    Domain.Neutral (Domain.Global _) _ ->
      pure False

    Domain.Neutral (Domain.Meta blockingMeta) _ ->
      throwError blockingMeta

    Domain.Lit _ ->
      pure True

    Domain.Con constr args -> do
      constrTypeTele <- fetch $ Query.ConstructorType constr
      let
        spine' =
          dropTypeArgs constrTypeTele $ toList args

      and <$> mapM (isPatternValue context . snd) spine'

    Domain.Glued _ _ value'' -> do
      value''' <- lift $ force value''
      isPatternValue context value'''

    Domain.Lam {} ->
      pure False

    Domain.Pi {} ->
      pure False

    Domain.Fun {} ->
      pure False

  where
    dropTypeArgs
      :: Telescope n t t' v
      -> [(Plicity, value)]
      -> [(Plicity, value)]
    dropTypeArgs tele args =
      case (tele, args) of
        (Telescope.Empty _, _) ->
          args

        (Telescope.Extend _ _ plicity1 tele', (plicity2, _):args')
          | plicity1 == plicity2 ->
            dropTypeArgs tele' args'

        _ ->
          panic "chooseBranch arg mismatch"

elaborateClauses
  :: Context v
  -> [Clause]
  -> Domain.Type
  -> M (Syntax.Term v)
elaborateClauses context clauses expectedType = do
  usedClauses <- newIORef mempty

  elaborateWithCoverage context Config
    { _expectedType = expectedType
    , _scrutinees =
      case clauses of
        firstClause:_ ->
          [(plicity, value) | Match value _ plicity _ _ <- _matches firstClause]

        _ ->
          mempty

    , _clauses = clauses
    , _usedClauses = usedClauses
    , _matchKind = Error.Clause
    }

elaborateSingle
  :: Context v
  -> Var
  -> Plicity
  -> Surface.Pattern
  -> Surface.Term
  -> Domain.Type
  -> M (Syntax.Term v)
elaborateSingle context scrutinee plicity pat@(Surface.Pattern patSpan _) rhs@(Surface.Term rhsSpan _) expectedType = do
  let
    scrutineeValue =
      Domain.var scrutinee

    scrutineeType =
      Context.lookupVarType scrutinee context

  usedClauses <- newIORef mempty

  elaborateWithCoverage context Config
    { _expectedType = expectedType
    , _scrutinees = pure (plicity, scrutineeValue)
    , _clauses =
      [ Clause
        { _span = Span.add patSpan rhsSpan
        , _matches = [Match scrutineeValue scrutineeValue plicity pat scrutineeType]
        , _rhs = rhs
        }
      ]
    , _usedClauses = usedClauses
    , _matchKind = Error.Lambda
    }

-------------------------------------------------------------------------------

elaborateWithCoverage :: Context v -> Config -> M (Syntax.Term v)
elaborateWithCoverage context config = do
  result <- elaborate context config
  let
    allClauseSpans =
      Set.fromList
        [ span
        | Clause span _ _ <- _clauses config
        ]
  usedClauseSpans <- readIORef (_usedClauses config)
  forM_ (Set.difference allClauseSpans usedClauseSpans) $ \span ->
    Context.report (Context.spanned span context) $ Error.RedundantMatch $ _matchKind config
  pure result

elaborate :: Context v -> Config -> M (Syntax.Term v)
elaborate context config = do
  clauses <- catMaybes <$> mapM (simplifyClause context) (_clauses config)
  let
    config' = config { _clauses = clauses }
  case clauses of
    [] -> do
      exhaustive <- anyM (uninhabitedScrutinee context . snd) $ _scrutinees config
      unless exhaustive $ do
        scrutinees <- forM (_scrutinees config) $ \(plicity, scrutinee) -> do
          patterns <- uncoveredScrutineePatterns context scrutinee
          pure $ (,) plicity <$> (Context.toPrettyablePattern context <$> patterns)
        Context.report context $ Error.NonExhaustivePatterns $ sequence scrutinees
      targetType <- Elaboration.readback context $ _expectedType config
      pure $ Syntax.App (Syntax.Global Builtin.fail) Explicit targetType

    firstClause:_ -> do
      let
        matches = _matches firstClause

      splitEqualityOr context config' matches $
        splitConstructorOr context config' matches $ do
          maybeInst <- solved context matches
          case maybeInst of
            Nothing -> do
              Context.report context $ Error.IndeterminateIndexUnification $ _matchKind config
              targetType <- Elaboration.readback context $ _expectedType config
              pure $ Syntax.App (Syntax.Global Builtin.fail) Explicit targetType

            Just inst -> do
              letBindPatternInstantiation context inst $ \context' -> do
                mapM_ (checkForcedPattern context') matches
                result <- Elaboration.check context' (_rhs firstClause) (_expectedType config)
                modifyIORef (_usedClauses config) $ Set.insert $ _span firstClause
                pure result

checkForcedPattern :: Context v -> Match -> M ()
checkForcedPattern context match =
  case match of
    Match value1 _ _ (Surface.Pattern span (Surface.Forced term)) type_ -> do
      let
        context' =
          Context.spanned span context

      term' <- Elaboration.check context' term type_
      value2 <- Elaboration.evaluate context term'
      _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid value1 value2
      pure ()

    _ ->
      pure ()

uncoveredScrutineePatterns
  :: Context v
  -> Domain.Value
  -> M [Pattern]
uncoveredScrutineePatterns context value = do
  value' <- Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var v) Domain.Empty -> do
      let
        covered =
          IntMap.lookupDefault mempty v $ Context.coveredConstructors context

        go :: Name.Qualified -> Telescope Binding Syntax.Type Syntax.ConstructorDefinitions v -> [Pattern]
        go typeName tele =
          case tele of
            Telescope.Empty (Syntax.ConstructorDefinitions constrDefs) -> do
              let
                uncoveredConstrDefs =
                  OrderedHashMap.differenceFromMap
                    constrDefs
                    (HashSet.toMap $
                      HashSet.map (\(Name.QualifiedConstructor _ constr) -> constr) covered
                    )

              foreach (OrderedHashMap.toList uncoveredConstrDefs) $ \(constr, type_) ->
                Pattern.Con
                  (Name.QualifiedConstructor typeName constr)
                  [ (plicity, Pattern.Wildcard)
                  | plicity <- Syntax.constructorFieldPlicities type_
                  ]

            Telescope.Extend _ _ _ tele' ->
              go typeName tele'

      case HashSet.toList covered of
        [] ->
          pure [Pattern.Wildcard]

        Name.QualifiedConstructor typeName _:_ -> do
          maybeDefinition <- fetch $ Query.ElaboratedDefinition typeName
          case maybeDefinition of
            Just (Syntax.DataDefinition _ tele, _) ->
              pure $ go typeName tele

            _ ->
              panic "uncoveredScrutineePatterns non-data"

    Domain.Neutral (Domain.Var _) (_ Domain.:> _) ->
      pure []

    Domain.Neutral (Domain.Meta _) _ ->
      pure []

    Domain.Neutral (Domain.Global _) _ ->
      pure []

    Domain.Lit lit ->
      pure [Pattern.Lit lit]

    Domain.Con constr args -> do
      constrTypeTele <- fetch $ Query.ConstructorType constr
      let
        spine' =
          dropTypeArgs constrTypeTele $ toList args

      spine'' <- forM spine' $ \(plicity, arg) -> do
        patterns <- uncoveredScrutineePatterns context arg
        pure $ (,) plicity <$> patterns
      pure $ Pattern.Con constr <$> sequence spine''

    Domain.Glued _ _ value'' -> do
      value''' <- force value''
      uncoveredScrutineePatterns context value'''

    Domain.Lam {} ->
      pure []

    Domain.Pi {} ->
      pure []

    Domain.Fun {} ->
      pure []
  where
    dropTypeArgs
      :: Telescope n t t' v
      -> [(Plicity, value)]
      -> [(Plicity, value)]
    dropTypeArgs tele args =
      case (tele, args) of
        (Telescope.Empty _, _) ->
          args

        (Telescope.Extend _ _ plicity1 tele', (plicity2, _):args')
          | plicity1 == plicity2 ->
            dropTypeArgs tele' args'

        _ ->
          panic "chooseBranch arg mismatch"

-------------------------------------------------------------------------------

simplifyClause :: Context v -> Clause -> M (Maybe Clause)
simplifyClause context clause = do
  maybeMatches <- runMaybeT $
    concat <$> mapM (simplifyMatch context) (_matches clause)
  case maybeMatches of
    Nothing ->
      pure Nothing

    Just matches' -> do
      maybeExpanded <- runMaybeT $ expandAnnotations context matches'
      case maybeExpanded of
        Nothing ->
          pure $ Just clause { _matches = matches' }

        Just expandedMatches ->
          simplifyClause context clause { _matches = expandedMatches }

simplifyMatch
  :: Context v
  -> Match
  -> MaybeT M [Match]
simplifyMatch context (Match value forcedValue plicity pat@(Surface.Pattern span unspannedPattern) type_) = do
  forcedValue' <- lift $ Context.forceHead context forcedValue
  let
    match' =
      Match value forcedValue' plicity pat type_
  case (forcedValue', unspannedPattern) of
    (Domain.Con constr args, Surface.ConOrVar _ name pats) -> do
      maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
      case maybeScopeEntry of
        Just scopeEntry
          | constr `HashSet.member` Scope.entryConstructors scopeEntry -> do
            matches' <- lift $ do
              constrType <- fetch $ Query.ConstructorType constr
              (patsType, patSpine) <-
                instantiateConstructorType
                  (Context.toEnvironment context)
                  (Telescope.fromVoid constrType)
                  (toList args)

              (matches', type') <- matchPrepatterns context patSpine pats patsType
              let
                context' =
                  Context.spanned span context
              _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid type_ type'
              pure matches'
            concat <$> mapM (simplifyMatch context) matches'

          | otherwise ->
            fail "Constructor mismatch"

        _ ->
          pure [match']

    (Domain.Lit lit, Surface.LitPattern lit')
      | lit == lit' ->
        pure []

      | otherwise ->
        fail "Literal mismatch"

    (Domain.Neutral (Domain.Var var) Domain.Empty, Surface.ConOrVar _ name _)
      | Just coveredConstrs <- IntMap.lookup var (Context.coveredConstructors context) -> do
        maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
        case maybeScopeEntry of
          Just scopeEntry -> do
            maybeConstr <- lift $ tryResolveConstructor context scopeEntry type_
            case maybeConstr of
              Just constr
                | HashSet.member constr coveredConstrs ->
                  fail "Constructor already covered"

                | otherwise ->
                  pure [match']

              Nothing ->
                pure [match']

          _ ->
            pure [match']

    (Domain.Neutral (Domain.Var var) Domain.Empty, Surface.LitPattern lit)
      | Just coveredLits <- IntMap.lookup var (Context.coveredLiterals context)
      , HashSet.member lit coveredLits ->
        fail "Literal already covered"

    _ ->
      pure [match']

instantiateConstructorType
  :: Domain.Environment v
  -> Telescope Binding Syntax.Type Syntax.Type v
  -> [(Plicity, Domain.Value)]
  -> M (Domain.Type, [(Plicity, Domain.Value)])
instantiateConstructorType env tele spine =
  case (tele, spine) of
    (Telescope.Empty constrType, _) -> do
      constrType' <- Evaluation.evaluate env constrType
      pure (constrType', spine)

    (Telescope.Extend _ _ plicity1 tele', (plicity2, arg):spine')
      | plicity1 == plicity2 -> do
        (env', _) <- Environment.extendValue env arg
        instantiateConstructorType env' tele' spine'

    _ ->
      panic $ "instantiateConstructorType: " <> show (tele, fst <$> spine)

matchPrepatterns
  :: Context v
  -> [(Plicity, Domain.Value)]
  -> [Surface.PlicitPattern]
  -> Domain.Type
  -> M ([Match], Domain.Type)
matchPrepatterns context values patterns type_ =
  case (patterns, values) of
    ([], []) ->
      pure ([], type_)

    (Surface.ExplicitPattern pat:patterns', (Explicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi _ domain Explicit targetClosure -> do
          target <- Evaluation.evaluateClosure targetClosure value
          explicitFunCase value values' pat patterns' domain target

        Domain.Fun domain Explicit target ->
          explicitFunCase value values' pat patterns' domain target

        _ ->
          panic "matchPrepatterns explicit non-pi"

    (Surface.ImplicitPattern _ namedPats:patterns', _)
      | HashMap.null namedPats ->
        matchPrepatterns context values patterns' type_

    (Surface.ImplicitPattern patSpan namedPats:patterns', (Implicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi binding domain Implicit targetClosure
          | let name = Binding.toName binding
          , HashMap.member name namedPats -> do
            target <- Evaluation.evaluateClosure targetClosure value
            (matches, type'') <-
              matchPrepatterns
                context
                values'
                (Surface.ImplicitPattern patSpan (HashMap.delete name namedPats) : patterns')
                target
            pure (Match value value Implicit (namedPats HashMap.! name) domain : matches, type'')

          | otherwise -> do
            target <- Evaluation.evaluateClosure targetClosure value
            matchPrepatterns context values' patterns target

        _ ->
          panic "matchPrepatterns implicit non-pi"

    (_, (Implicit, value):values') -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi _ _ Implicit targetClosure -> do
          target <- Evaluation.evaluateClosure targetClosure value
          matchPrepatterns context values' patterns target

        Domain.Fun _ Implicit target ->
          matchPrepatterns context values' patterns target

        _ ->
          panic "matchPrepatterns implicit non-pi 2"

    (_, (Constraint, value):values') -> do
      type' <- Context.forceHead context type_
      let
        go domain target = do
          (matches, type'') <- matchPrepatterns context values' patterns target
          let
            pattern_ =
              Surface.Pattern (Context.span context) Surface.WildcardPattern
          pure (Match value value Constraint pattern_ domain : matches, type'')

      case type' of
        Domain.Pi _ domain Constraint targetClosure -> do
          target <- Evaluation.evaluateClosure targetClosure value
          go domain target

        Domain.Fun domain Constraint target ->
          go domain target

        _ ->
          panic "matchPrepatterns constraint non-pi"

    (pat:_, []) -> do
      Context.report (Context.spanned (Surface.plicitPatternSpan pat) context) $ Error.PlicityMismatch Error.Field Error.Extra
      pure ([], type_)

    ([], (Explicit, _):_) -> do
      Context.report context $ Error.PlicityMismatch Error.Field $ Error.Missing Explicit
      matchPrepatterns context values [Surface.ExplicitPattern $ Surface.Pattern (Context.span context) Surface.WildcardPattern] type_

    (Surface.ImplicitPattern patSpan _:patterns', (Explicit, _):_) -> do
      Context.report (Context.spanned patSpan context) $ Error.PlicityMismatch Error.Field (Error.Mismatch Explicit Implicit)
      matchPrepatterns context values patterns' type_

  where
    explicitFunCase value values' pat patterns' domain target = do
      (matches, type'') <- matchPrepatterns context values' patterns' target
      pure (Match value value Explicit pat domain : matches, type'')

type PatternInstantiation = Tsil (Bindings, Domain.Value, Domain.Type)

letBindPatternInstantiation :: Context v -> PatternInstantiation -> (forall v'. Context v' -> M (Syntax.Term v')) -> M (Syntax.Term v)
letBindPatternInstantiation context inst k =
  case inst of
    Tsil.Empty ->
      k context

    inst' Tsil.:> (bindings, value, type_) -> do
      (context', _) <- Context.extendPreDef context (Bindings.toName bindings) value type_
      result <- letBindPatternInstantiation context' inst' k
      term <- Elaboration.readback context value
      type' <- Elaboration.readback context type_
      pure $ Syntax.Let bindings term type' result

extendWithPatternInstantation :: Context v -> PatternInstantiation -> (forall v'. Context v' -> MaybeT M a) -> MaybeT M a
extendWithPatternInstantation context inst k =
  case inst of
    Tsil.Empty ->
      k context

    inst' Tsil.:> (bindings, value, type_) -> do
      (context', _) <- lift $ Context.extendPreDef context (Bindings.toName bindings) value type_
      extendWithPatternInstantation context' inst' k

expandAnnotations
  :: Context v
  -> [Match]
  -> MaybeT M [Match]
expandAnnotations context matches =
  case matches of
    [] ->
      fail "expanded nothing"

    match:matches' -> do
      maybeInst <- lift $ runMaybeT $ matchInstantiation context match
      case maybeInst of
        Just inst ->
          extendWithPatternInstantation context inst $ \context' -> do
            matches'' <- expandAnnotations context' matches'
            pure $ match : matches''

        Nothing ->
          case match of
            Match value forcedValue plicity (Surface.Pattern span (Surface.Anno pat annoType)) type_ -> do
              lift $ do
                annoType' <- Elaboration.check context annoType Builtin.Type
                annoType'' <- Elaboration.evaluate context annoType'
                let
                  context' =
                    Context.spanned span context
                _ <- Context.try_ context' $ Unification.unify context' Flexibility.Rigid annoType'' type_
                pure ()
              pure $ Match value forcedValue plicity pat type_ : matches'

            _ ->
              fail "couldn't create instantitation for prefix"

matchInstantiation :: Context v -> Match -> MaybeT M PatternInstantiation
matchInstantiation context match =
  case match of
    (Match _ _ _ (Surface.Pattern _ Surface.WildcardPattern) _) ->
      pure mempty

    (Match value _ _ (Surface.Pattern span (Surface.ConOrVar _ prename@(Name.Pre name) [])) type_) -> do
      maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) prename
      if HashSet.null $ foldMap Scope.entryConstructors maybeScopeEntry then
        pure $ pure (Bindings.Spanned $ pure (span, Name name), value, type_)

      else
        fail "No match instantiation"

    (Match _ _ _ (Surface.Pattern _ (Surface.Forced _)) _) ->
      pure mempty

    _ ->
      fail "No match instantitation"

solved :: Context v -> [Match] -> M (Maybe PatternInstantiation)
solved context =
  runMaybeT . fmap mconcat . traverse (matchInstantiation context)

-------------------------------------------------------------------------------

splitConstructorOr
  :: Context v
  -> Config
  -> [Match]
  -> M (Syntax.Term v)
  -> M (Syntax.Term v)
splitConstructorOr context config matches k =
  case matches of
    [] ->
      k

    match:matches' ->
      case match of
        Match
          scrutinee
          (Domain.Neutral (Domain.Var var) Domain.Empty)
          _
          (Surface.Pattern span (Surface.ConOrVar _ name _))
          type_ -> do
            maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
            case maybeScopeEntry of
              Just scopeEntry -> do
                maybeConstr <- tryResolveConstructor context scopeEntry type_
                case maybeConstr of
                  Just constr ->
                    splitConstructor context config scrutinee var span constr type_

                  Nothing ->
                    splitConstructorOr context config matches' k

              Nothing ->
                splitConstructorOr context config matches' k

        Match
          scrutinee
          (Domain.Neutral (Domain.Var var) Domain.Empty)
          _
          (Surface.Pattern span (Surface.LitPattern lit))
          type_ ->
            splitLiteral context config scrutinee var span lit type_

        _ ->
          splitConstructorOr context config matches' k

splitConstructor
  :: Context v
  -> Config
  -> Domain.Value
  -> Var
  -> Span.Relative
  -> Name.QualifiedConstructor
  -> Domain.Type
  -> M (Syntax.Term v)
splitConstructor outerContext config scrutineeValue scrutineeVar span (Name.QualifiedConstructor typeName _) outerType = do
  maybeDefinition <- fetch $ Query.ElaboratedDefinition typeName
  case maybeDefinition of
    Just (Syntax.DataDefinition _ tele, _) -> do
      tele' <- Evaluation.evaluateConstructorDefinitions (Environment.empty $ Context.scopeKey outerContext) tele
      outerType' <- Context.forceHead outerContext outerType
      case outerType' of
        Domain.Neutral (Domain.Global typeName') spine
          | typeName == typeName'
          , Just params <- Domain.appsView spine ->
            goParams (Context.spanned span outerContext) (toList params) mempty tele'

        _ -> do
          typeType <- fetch $ Query.ElaboratedType typeName
          typeType' <- Evaluation.evaluate (Environment.empty $ Context.scopeKey outerContext) typeType
          let
            -- Ensure the metas don't depend on the scrutineeVar, because that
            -- is guaranteed to lead to circularity when solving scrutineeVar
            -- later.
            contextWithoutScrutineeVar =
              outerContext
                { Context.boundVars = IntSeq.delete scrutineeVar $ Context.boundVars outerContext
                }
          (metas, _) <- Elaboration.insertMetas contextWithoutScrutineeVar Elaboration.UntilTheEnd typeType'
          f <- Unification.tryUnify outerContext (Domain.Neutral (Domain.Global typeName) $ Domain.Apps $ fromList metas) outerType
          result <- goParams (Context.spanned span outerContext) metas mempty tele'
          pure $ f result

    _ ->
      panic "splitConstructor no data definition"
  where
    goParams
      :: Context v
      -> [(Plicity, Domain.Value)]
      -> Tsil (Plicity, Domain.Value)
      -> Domain.Telescope (OrderedHashMap Name.Constructor Domain.Type)
      -> M (Syntax.Type v)
    goParams context params conArgs dataTele =
      case (params, dataTele) of
        ([], Domain.Telescope.Empty constructors) -> do
          matchedConstructors <-
            OrderedHashMap.fromListWith (<>) . concat . takeWhile (not . null) <$>
              mapM
                (findVarConstructorMatches context scrutineeVar . _matches)
                (_clauses config)

          branches <- forM (OrderedHashMap.toList matchedConstructors) $ \(qualifiedConstr@(Name.QualifiedConstructor _ constr), patterns) -> do
            let
              constrType =
                OrderedHashMap.lookupDefault
                  (panic "Matching constrType")
                  constr
                  constructors

              conSpans =
                fst <$> patterns

            tele <- goConstrFields context qualifiedConstr conArgs constrType $ snd <$> patterns
            pure (constr, (conSpans, tele))


          defaultBranch <-
            if OrderedHashMap.size matchedConstructors == length constructors then
              pure Nothing

            else
              Just <$> elaborate context
                { Context.coveredConstructors =
                  IntMap.insertWith (<>) scrutineeVar (HashSet.fromMap $ void $ OrderedHashMap.toMap matchedConstructors) $
                  Context.coveredConstructors context
                }
                config

          scrutinee <- Elaboration.readback context scrutineeValue

          pure $ Syntax.Case scrutinee (Syntax.ConstructorBranches typeName $ OrderedHashMap.fromList branches) defaultBranch

        ((plicity1, param):params', Domain.Telescope.Extend _ _ plicity2 targetClosure)
          | plicity1 == plicity2 -> do
            target <- targetClosure param
            goParams context params' (conArgs Tsil.:> (implicitise plicity1, param)) target

        _ ->
          panic "goParams mismatch"

    goConstrFields
      :: Context v
      -> Name.QualifiedConstructor
      -> Tsil (Plicity, Domain.Value)
      -> Domain.Type
      -> [[Surface.PlicitPattern]]
      -> M (Telescope Bindings Syntax.Type Syntax.Term v)
    goConstrFields context constr conArgs type_ patterns =
      case type_ of
        Domain.Pi piBinding domain plicity targetClosure -> do
          let
            piName =
              Binding.toName piBinding
          domain'' <- Elaboration.readback context domain
          (bindings, patterns') <-
            case plicity of
              Explicit ->
                SuggestedName.nextExplicit context patterns

              Implicit ->
                SuggestedName.nextImplicit context piName patterns

              Constraint ->
                pure (Bindings.Unspanned piName, patterns)

          (context' , fieldVar) <- Context.extendBefore context scrutineeVar bindings domain
          let
            fieldValue =
              Domain.var fieldVar

            conArgs' =
              conArgs Tsil.:> (plicity, fieldValue)

          target <- Evaluation.evaluateClosure targetClosure fieldValue
          tele <- goConstrFields context' constr conArgs' target patterns'
          pure $ Telescope.Extend bindings domain'' plicity tele

        Domain.Fun domain plicity target -> do
          domain'' <- Elaboration.readback context domain
          (bindings, patterns') <-
            case plicity of
             Explicit ->
               SuggestedName.nextExplicit context patterns

             Implicit ->
               SuggestedName.nextImplicit context "x" patterns

             Constraint ->
               pure (Bindings.Unspanned "x", patterns)
          (context' , fieldVar) <- Context.extendBefore context scrutineeVar bindings domain
          let
            fieldValue =
              Domain.var fieldVar

            conArgs' =
              conArgs Tsil.:> (plicity, fieldValue)

          tele <- goConstrFields context' constr conArgs' target patterns'
          pure $ Telescope.Extend bindings domain'' plicity tele

        _ -> do
          let
            context' =
              Context.defineWellOrdered context scrutineeVar $ Domain.Con constr conArgs
          result <- elaborate context' config
          pure $ Telescope.Empty result

findVarConstructorMatches
  :: Context v
  -> Var
  -> [Match]
  -> M [(Name.QualifiedConstructor, [(Span.Relative, [Surface.PlicitPattern])])]
findVarConstructorMatches context var matches =
    case matches of
      [] ->
        pure []

      Match _ (Domain.Neutral (Domain.Var var') Domain.Empty) _ (Surface.Pattern _ (Surface.ConOrVar span name patterns)) type_:matches'
        | var == var' -> do
          maybeScopeEntry <- fetch $ Query.ResolvedName (Context.scopeKey context) name
          case maybeScopeEntry of
            Just scopeEntry -> do
              maybeConstr <- tryResolveConstructor context scopeEntry type_
              case maybeConstr of
                Just constr ->
                  ((constr, [(span, patterns)]) :) <$> findVarConstructorMatches context var matches'

                Nothing ->
                  findVarConstructorMatches context var matches'

            Nothing ->
              findVarConstructorMatches context var matches'

      _:matches' ->
        findVarConstructorMatches context var matches'

splitLiteral
  :: Context v
  -> Config
  -> Domain.Value
  -> Var
  -> Span.Relative
  -> Literal
  -> Domain.Type
  -> M (Syntax.Term v)
splitLiteral context config scrutineeValue scrutineeVar span lit outerType = do
  matchedLiterals <-
    OrderedHashMap.fromListWith (<>) . concat . takeWhile (not . null) <$>
      mapM
        (findVarLiteralMatches context scrutineeVar . _matches)
        (_clauses config)

  f <- Unification.tryUnify (Context.spanned span context) (Elaboration.inferLiteral lit) outerType

  branches <- forM (OrderedHashMap.toList matchedLiterals) $ \(int, spans) -> do
    let
      context' =
        Context.defineWellOrdered context scrutineeVar $ Domain.Lit int
    result <- elaborate context' config
    pure (int, (spans, f result))

  defaultBranch <-
    Just <$> elaborate context
      { Context.coveredLiterals =
        IntMap.insertWith (<>) scrutineeVar (HashSet.fromMap $ void $ OrderedHashMap.toMap matchedLiterals) $
        Context.coveredLiterals context
      }
      config

  scrutinee <- Elaboration.readback context scrutineeValue

  pure $ f $ Syntax.Case scrutinee (Syntax.LiteralBranches $ OrderedHashMap.fromList branches) defaultBranch

findVarLiteralMatches
  :: Context v
  -> Var
  -> [Match]
  -> M [(Literal, [Span.Relative])]
findVarLiteralMatches context var matches =
    case matches of
      [] ->
        pure []

      Match _ (Domain.Neutral (Domain.Var var') Domain.Empty) _ (Surface.Pattern span (Surface.LitPattern lit)) _:matches'
        | var == var' ->
          ((lit, [span]) :) <$> findVarLiteralMatches context var matches'

      _:matches' ->
        findVarLiteralMatches context var matches'

-------------------------------------------------------------------------------

splitEqualityOr
  :: Context v
  -> Config
  -> [Match]
  -> M (Syntax.Term v)
  -> M (Syntax.Term v)
splitEqualityOr context config matches k =
  case matches of
    [] ->
      k

    match:matches' ->
      case match of
        Match
          _
          (Domain.Neutral (Domain.Var var) Domain.Empty)
          _
          (Surface.Pattern _ Surface.WildcardPattern)
          (Builtin.Equals type_ value1 value2) -> do
            unificationResult <- try $ Indices.unify context Flexibility.Rigid mempty value1 value2
            case unificationResult of
              Left Indices.Nope ->
                elaborate context config
                  { _clauses = drop 1 $ _clauses config
                  }

              Left Indices.Dunno ->
                splitEqualityOr context config matches' k

              Right context' -> do
                context'' <- Context.define context' var $ Builtin.Refl type_ value1 value2
                result <- elaborate context'' config
                scrutinee <- Elaboration.readback context'' $ Domain.var var
                pure $
                  Syntax.Case scrutinee
                  (Syntax.ConstructorBranches
                    Builtin.EqualsName
                    (OrderedHashMap.fromList [(Name.unqualifyConstructor Builtin.ReflName, ([], Telescope.Empty result))])
                  )
                  Nothing

        _ ->
          splitEqualityOr context config matches' k

-------------------------------------------------------------------------------

uninhabitedScrutinee :: Context v -> Domain.Value -> M Bool
uninhabitedScrutinee context value = do
  value' <- Context.forceHead context value
  case value' of
    Domain.Neutral (Domain.Var var) (Domain.appsView -> Just args) -> do
      let
        varType =
          Context.lookupVarType var context
      type_ <- Context.instantiateType context varType $ toList args
      uninhabitedType context 1 (IntMap.lookupDefault mempty var $ Context.coveredConstructors context) type_

    Domain.Con constr constructorArgs -> do
      constrType <- fetch $ Query.ConstructorType constr
      let
        args = snd <$> drop (Telescope.length constrType) (toList constructorArgs)
      anyM (uninhabitedScrutinee context) args

    _ ->
      pure False

uninhabitedType
  :: Context v
  -> Int
  -> HashSet Name.QualifiedConstructor
  -> Domain.Type
  -> M Bool
uninhabitedType context fuel coveredConstructors type_ = do
  type' <- Context.forceHead context type_
  case type' of
    Builtin.Equals _ value1 value2 -> do
      result <- try $ Indices.unify context Flexibility.Rigid mempty value1 value2
      pure $ case result of
        Left Indices.Nope ->
          True

        Left Indices.Dunno ->
          False

        Right _ ->
          False

    Domain.Neutral (Domain.Global global) (Domain.appsView -> Just args) -> do
      maybeDefinitions <- fetch $ Query.ElaboratedDefinition global
      case maybeDefinitions of
        Just (Syntax.DataDefinition _ tele, _) -> do
          tele' <- Evaluation.evaluateConstructorDefinitions (Environment.empty $ Context.scopeKey context) tele
          tele'' <- Domain.Telescope.apply tele' $ toList args
          case tele'' of
            Domain.Telescope.Empty constructors -> do
              let
                qualifiedConstructors =
                  OrderedHashMap.fromList
                    [ (Name.QualifiedConstructor global constr, constrType)
                    | (constr, constrType) <- OrderedHashMap.toList constructors
                    ]

                uncoveredConstructorTypes =
                  toList $
                  OrderedHashMap.differenceFromMap qualifiedConstructors (HashSet.toMap coveredConstructors)

              allM (uninhabitedConstrType context fuel) uncoveredConstructorTypes

            _ ->
              pure False

        _ ->
          pure False

    _ ->
      pure False

uninhabitedConstrType :: Context v -> Int -> Domain.Type -> M Bool
uninhabitedConstrType context fuel type_ =
  case fuel of
    0 ->
      pure False

    _ -> do
      type' <- Context.forceHead context type_
      case type' of
        Domain.Pi binding domain _ targetClosure -> do
          uninhabited <- uninhabitedType context (fuel - 1) mempty domain
          if uninhabited then
            pure True

          else do
            (context', var) <- Context.extend context (Binding.toName binding) domain
            target <- Evaluation.evaluateClosure targetClosure $ Domain.var var
            uninhabitedConstrType context' fuel target

        Domain.Fun domain _ target -> do
          uninhabited <- uninhabitedType context (fuel - 1) mempty domain
          if uninhabited then
            pure True

          else
            uninhabitedConstrType context fuel target

        _ ->
          pure False

tryResolveConstructor :: Context v -> Scope.Entry -> Domain.Type -> M (Maybe Name.QualifiedConstructor)
tryResolveConstructor context scopeEntry type_ = do
  let
    entryConstrs =
      Scope.entryConstructors scopeEntry

  resolution <- Elaboration.resolveConstructor entryConstrs mempty $ Elaboration.getExpectedTypeName context type_
  case resolution of
    Left _ ->
      pure Nothing

    Right (Elaboration.ResolvedConstructor constr) ->
      pure $ Just constr

    Right Elaboration.ResolvedData {} ->
      pure Nothing

    Right Elaboration.Ambiguous {} ->
      pure Nothing
