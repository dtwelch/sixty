{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LanguageServer.Completion where

import Control.Monad.Trans.Maybe
import qualified Core.Domain as Domain
import qualified Core.Evaluation as Evaluation
import qualified Core.Syntax as Syntax
import qualified Core.TypeOf as TypeOf
import Data.EnumMap (EnumMap)
import qualified Data.EnumMap as EnumMap
import qualified Data.HashMap.Lazy as HashMap
import Data.IORef.Lifted
import qualified Elaboration
import Elaboration.Context (Context)
import qualified Elaboration.Context as Context
import qualified Error.Hydrated as Error
import qualified Language.LSP.Types as LSP
import qualified LanguageServer.CursorAction as CursorAction
import Monad
import Name (Name (Name))
import qualified Name
import Plicity
import qualified Position
import Prettyprinter ((<+>))
import Protolude hiding (catch, evaluate, moduleName)
import Query (Query)
import qualified Query
import qualified Query.Mapped as Mapped
import Rock
import qualified Scope
import Var (Var)

complete :: FilePath -> Position.LineColumn -> Task Query (Maybe [LSP.CompletionItem])
complete filePath (Position.LineColumn line column) =
  CursorAction.cursorAction filePath (Position.LineColumn line $ max 0 $ column - 1) \item _ ->
    case item of
      CursorAction.Import _ ->
        empty
      CursorAction.Term itemContext context varPositions _ -> do
        names <- lift $ getUsableNames itemContext context varPositions
        lift $
          forM names \(name, value, kind) -> do
            type_ <- TypeOf.typeOf context value
            type' <- Elaboration.readback context type_
            prettyType <- Error.prettyPrettyableTerm 0 =<< Context.toPrettyableTerm context type'
            pure
              LSP.CompletionItem
                { _label = name
                , _kind = Just kind
                , _detail = Just $ show $ ":" <+> prettyType
                , _documentation = Nothing
                , _deprecated = Nothing
                , _preselect = Nothing
                , _sortText = Nothing
                , _filterText = Nothing
                , _insertText = Nothing
                , _insertTextFormat = Nothing
                , _insertTextMode = Nothing
                , _textEdit = Nothing
                , _additionalTextEdits = Nothing
                , _commitCharacters = Nothing
                , _command = Nothing
                , _xdata = Nothing
                , _tags = mempty
                }

questionMark :: FilePath -> Position.LineColumn -> Task Query (Maybe [LSP.CompletionItem])
questionMark filePath (Position.LineColumn line column) =
  CursorAction.cursorAction filePath (Position.LineColumn line $ max 0 $ column - 1) \item _ ->
    case item of
      CursorAction.Import _ ->
        empty
      CursorAction.Term itemContext context varPositions termUnderCursor -> do
        valueUnderCursor <- lift $ Elaboration.evaluate context termUnderCursor
        typeUnderCursor <- lift $ TypeOf.typeOf context valueUnderCursor
        typeUnderCursor' <- lift $ Elaboration.readback context typeUnderCursor
        hPutStrLn stderr (show varPositions :: Text)
        prettyTypeUnderCursor <- lift $ Error.prettyPrettyableTerm 0 =<< Context.toPrettyableTerm context typeUnderCursor'
        names <- lift $ getUsableNames itemContext context varPositions

        metasBefore <- readIORef context.metas
        lift $
          fmap concat $
            forM names \(name, value, kind) -> do
              writeIORef context.metas metasBefore
              type_ <- TypeOf.typeOf context value
              (maxArgs, _) <- Elaboration.insertMetas context Elaboration.UntilTheEnd type_
              metasBefore' <- readIORef context.metas
              maybeArgs <- runMaybeT $
                asum $
                  foreach (inits maxArgs) \args -> do
                    writeIORef context.metas metasBefore'
                    appliedValue <- lift $ foldM (\fun (plicity, arg) -> Evaluation.apply fun plicity arg) value args
                    appliedType <- lift $ TypeOf.typeOf context appliedValue
                    MaybeT $ do
                      isSubtype <- Elaboration.isSubtype context appliedType typeUnderCursor
                      pure $ if isSubtype then Just () else Nothing
                    pure args

              pure $ case maybeArgs of
                Nothing ->
                  -- typeUnderCursor' <- Elaboration.readback context typeUnderCursor
                  -- type' <- Elaboration.readback context type_
                  -- prettyType <- Error.prettyPrettyableTerm 0 $ Context.toPrettyableTerm context type'
                  -- prettyTypeUnderCursor <- Error.prettyPrettyableTerm 0 $ Context.toPrettyableTerm context typeUnderCursor'
                  -- Text.hPutStrLn stderr $ "nothing " <> show prettyType
                  -- Text.hPutStrLn stderr $ "nothing toc " <> show prettyTypeUnderCursor
                  []
                Just args -> do
                  let explicitArgs =
                        filter ((== Explicit) . fst) args
                  pure
                    LSP.CompletionItem
                      { _label = name
                      , _kind = Just kind
                      , _detail = Just $ show $ ":" <+> prettyTypeUnderCursor
                      , _documentation = Nothing
                      , _deprecated = Nothing
                      , _preselect = Nothing
                      , _sortText = Nothing
                      , _filterText = Nothing
                      , _insertText = Nothing
                      , _insertTextFormat = Just LSP.Snippet
                      , _insertTextMode = Nothing
                      , _textEdit =
                          Just $
                            LSP.CompletionEditText
                              LSP.TextEdit
                                { _range =
                                    LSP.Range
                                      { _start =
                                          LSP.Position
                                            { _line = fromIntegral line
                                            , _character = fromIntegral $ column - 1
                                            }
                                      , _end =
                                          LSP.Position
                                            { _line = fromIntegral line
                                            , _character = fromIntegral column
                                            }
                                      }
                                , _newText =
                                    (if null explicitArgs then "" else "(")
                                      <> name
                                      <> mconcat
                                        [ " ${" <> show (n :: Int) <> ":?}"
                                        | (n, _) <- zip [1 ..] explicitArgs
                                        ]
                                      <> (if null explicitArgs then "" else ")")
                                }
                      , _additionalTextEdits = Nothing
                      , _commitCharacters = Nothing
                      , _command = Nothing
                      , _xdata = Nothing
                      , _tags = mempty
                      }

getUsableNames :: CursorAction.ItemContext -> Context v -> EnumMap Var value -> M [(Text, Domain.Value, LSP.CompletionItemKind)]
getUsableNames itemContext context varPositions = do
  hPutStrLn stderr $ "getUsableNames " ++ show itemContext
  locals <- case itemContext of
    CursorAction.ExpressionContext ->
      forM (EnumMap.toList varPositions) \(var, _) -> do
        let Name text =
              Context.lookupVarName var context
        pure (text, Domain.var var, LSP.CiVariable)
    CursorAction.PatternContext ->
      pure []
    CursorAction.DefinitionContext ->
      pure []

  let module_ = Context.moduleName context
  (_, moduleScope) <- fetch $ Query.ModuleScope module_
  importedScopeEntries <- fetch $ Query.ImportedNames module_ Mapped.Map
  let scopeEntries =
        moduleScope <> importedScopeEntries

  imports <- forM (HashMap.toList scopeEntries) \(Name.Surface name, entry) ->
    case entry of
      Scope.Name global -> do
        let go = do
              (definition, _) <- fetch $ Query.ElaboratedDefinition global
              pure
                [
                  ( name
                  , Domain.global global
                  , case definition of
                      Syntax.DataDefinition {} -> LSP.CiEnum
                      Syntax.ConstantDefinition {} -> LSP.CiConstant
                      Syntax.TypeDeclaration {} -> LSP.CiConstant
                  )
                ]
        case itemContext of
          CursorAction.ExpressionContext ->
            go
          CursorAction.PatternContext ->
            pure []
          CursorAction.DefinitionContext ->
            go
      Scope.Constructors constrs datas -> do
        let go =
              pure $
                case toList datas of
                  [data_] ->
                    [(name, Domain.global data_, LSP.CiEnum)]
                  _ ->
                    []
                  <> [ (name, Domain.con con, LSP.CiEnumMember)
                     | con <- toList constrs
                     ]
        case itemContext of
          CursorAction.ExpressionContext ->
            go
          CursorAction.PatternContext ->
            go
          CursorAction.DefinitionContext ->
            pure []
      Scope.Ambiguous _ _ ->
        pure []
  pure $ locals <> concat imports
