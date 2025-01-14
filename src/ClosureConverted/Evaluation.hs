{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module ClosureConverted.Evaluation where

import qualified ClosureConverted.Domain as Domain
import qualified ClosureConverted.Syntax as Syntax
import Control.Exception.Lifted (try)
import qualified Data.OrderedHashMap as OrderedHashMap
import Data.Some (Some)
import qualified Data.Tsil as Tsil
import qualified Environment
import GHC.Exts (fromList)
import qualified Index
import Literal (Literal)
import Monad
import qualified Name
import Protolude hiding (evaluate, force, head, try)
import Query (Query)
import qualified Query
import Rock
import Telescope (Telescope)
import qualified Telescope

evaluate :: Domain.Environment v -> Syntax.Term v -> M Domain.Value
evaluate env term =
  case term of
    Syntax.Var index -> do
      let var =
            Environment.lookupIndexVar index env

      pure $
        case Environment.lookupVarValue var env of
          Nothing ->
            Domain.var var
          Just value
            | Index.Succ index > Environment.glueableBefore env ->
                Domain.Glued (Domain.Var var) mempty value
            | otherwise ->
                value
    Syntax.Global name -> do
      maybeDefinition <- fetchVisibleDefinition name
      case maybeDefinition of
        Just (Syntax.ConstantDefinition term') -> do
          value <- lazy $ evaluate Environment.empty term'
          pure $ Domain.Glued (Domain.Global name) mempty $ Domain.Lazy value
        _ ->
          pure $ Domain.global name
    Syntax.Con con params args ->
      Domain.Con con <$> mapM (evaluate env) params <*> mapM (evaluate env) args
    Syntax.Lit lit ->
      pure $ Domain.Lit lit
    Syntax.Let _ term' _ body -> do
      term'' <- evaluate env term'
      (env', _) <- Environment.extendValue env term''
      evaluate env' body
    Syntax.Function tele ->
      pure $ Domain.Function tele
    Syntax.Apply name args -> do
      args' <- mapM (evaluate env) args
      apply env (Domain.global name) args'
    Syntax.Pi name domain target -> do
      domain' <- evaluate env domain
      pure $ Domain.Pi name domain' $ Domain.Closure env target
    Syntax.Closure global args -> do
      args' <- mapM (evaluate env) args
      pure $ Domain.Neutral (Domain.Global global) $ Domain.App <$> fromList args'
    Syntax.ApplyClosure fun args -> do
      fun' <- evaluate env fun
      args' <- mapM (evaluate env) args
      apply env fun' args'
    Syntax.Case scrutinee branches defaultBranch -> do
      scrutineeValue <- evaluate env scrutinee
      case_ scrutineeValue $ Domain.Branches env branches defaultBranch

chooseConstructorBranch
  :: Domain.Environment v
  -> Name.QualifiedConstructor
  -> [Domain.Value]
  -> Syntax.ConstructorBranches v
  -> Maybe (Syntax.Term v)
  -> M Domain.Value
chooseConstructorBranch outerEnv (Name.QualifiedConstructor _ constr) outerArgs branches defaultBranch =
  case (OrderedHashMap.lookup constr branches, defaultBranch) of
    (Nothing, Nothing) ->
      panic "chooseBranch no branches"
    (Nothing, Just branch) ->
      evaluate outerEnv branch
    (Just tele, _) ->
      go outerEnv outerArgs tele
  where
    go
      :: Domain.Environment v
      -> [Domain.Value]
      -> Telescope name Syntax.Type Syntax.Term v
      -> M Domain.Value
    go env args tele =
      case (args, tele) of
        ([], Telescope.Empty branch) ->
          evaluate env branch
        (arg : args', Telescope.Extend _ _ _ target) -> do
          (env', _) <- Environment.extendValue env arg
          go env' args' target
        _ ->
          panic "chooseConstructorBranch mismatch"

chooseLiteralBranch
  :: Domain.Environment v
  -> Literal
  -> Syntax.LiteralBranches v
  -> Maybe (Syntax.Term v)
  -> M Domain.Value
chooseLiteralBranch outerEnv lit branches defaultBranch =
  case (OrderedHashMap.lookup lit branches, defaultBranch) of
    (Nothing, Nothing) ->
      panic "chooseLiteralBranch no branches"
    (Nothing, Just branch) ->
      evaluate outerEnv branch
    (Just branch, _) ->
      evaluate outerEnv branch

apply
  :: Domain.Environment v
  -> Domain.Value
  -> [Domain.Value]
  -> M Domain.Value
apply env fun args =
  case fun of
    Domain.Neutral hd@(Domain.Global global) spine@(Domain.groupSpine -> [Domain.GroupedApps funArgs]) -> do
      let neutral =
            Domain.Neutral hd $ spine <> (Domain.App <$> fromList args)

      maybeDefinition <- fetchVisibleDefinition global
      case maybeDefinition of
        Just (Syntax.FunctionDefinition tele) -> do
          maybeResult <- applyFunction env (Telescope.fromVoid tele) (funArgs <> args)
          pure $ fromMaybe neutral maybeResult
        _ ->
          pure neutral
    Domain.Glued hd spine value -> do
      appliedValue <- apply env value args
      pure $ Domain.Glued hd (spine <> (Domain.App <$> fromList args)) appliedValue
    Domain.Lazy lazyValue -> do
      lazyValue' <- lazy $ do
        value' <- force lazyValue
        apply env value' args
      pure $ Domain.Lazy lazyValue'
    Domain.Neutral hd spine ->
      pure $ Domain.Neutral hd $ spine <> (Domain.App <$> fromList args)
    _ ->
      panic "applying non-function"

applyFunction
  :: Domain.Environment v
  -> Telescope name type_ Syntax.Term v
  -> [Domain.Value]
  -> M (Maybe Domain.Value)
applyFunction env tele args =
  case (tele, args) of
    (Telescope.Empty body, _) -> do
      body' <- evaluate env body
      Just
        <$> case args of
          [] ->
            pure body'
          _ ->
            apply env body' args
    (Telescope.Extend _ _ _ tele', arg : args') -> do
      (env', _) <- Environment.extendValue env arg
      applyFunction env' tele' args'
    _ ->
      pure Nothing

applyTelescope
  :: Domain.Environment v
  -> Telescope name type_ term v
  -> [Domain.Value]
  -> (forall v'. Domain.Environment v' -> term v' -> M a)
  -> M (Maybe a)
applyTelescope env tele args k =
  case (tele, args) of
    (Telescope.Empty body, []) -> do
      Just <$> k env body
    (Telescope.Extend _ _ _ tele', arg : args') -> do
      (env', _) <- Environment.extendValue env arg
      applyTelescope env' tele' args' k
    _ ->
      pure Nothing

case_ :: Domain.Value -> Domain.Branches -> M Domain.Value
case_ scrutinee branches@(Domain.Branches env branches' defaultBranch) =
  case (scrutinee, branches') of
    (Domain.Con constr _params args, Syntax.ConstructorBranches _ constructorBranches) ->
      chooseConstructorBranch env constr args constructorBranches defaultBranch
    (Domain.Lit lit, Syntax.LiteralBranches literalBranches) ->
      chooseLiteralBranch env lit literalBranches defaultBranch
    (Domain.Neutral head spine, _) ->
      pure $ Domain.Neutral head $ spine Tsil.:> Domain.Case branches
    (Domain.Glued hd spine value, _) -> do
      casedValue <- case_ value branches
      pure $ Domain.Glued hd (spine Tsil.:> Domain.Case branches) casedValue
    (Domain.Lazy lazyValue, _) -> do
      lazyValue' <- lazy $ do
        value <- force lazyValue
        case_ value branches
      pure $ Domain.Lazy lazyValue'
    _ ->
      panic "ClosureConverted.Evaluation: casing non-constructor"

evaluateClosure :: Domain.Closure -> Domain.Value -> M Domain.Value
evaluateClosure (Domain.Closure env body) argument = do
  (env', _) <- Environment.extendValue env argument
  evaluate env' body

fetchVisibleDefinition :: Name.Lifted -> M (Maybe Syntax.Definition)
fetchVisibleDefinition name = do
  result <- try $ fetch $ Query.ClosureConverted name
  pure $ case result of
    Right def -> Just def
    Left (Cyclic (_ :: Some Query)) ->
      Nothing

-- | Evaluate the head of a value through glued values.
forceHead
  :: Domain.Value
  -> M Domain.Value
forceHead value =
  case value of
    Domain.Glued _ _ value' ->
      forceHead value'
    Domain.Lazy lazyValue -> do
      value' <- force lazyValue
      forceHead value'
    _ ->
      pure value
