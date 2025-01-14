{-# LANGUAGE FlexibleContexts #-}

module LanguageServer.LineColumns where

import qualified Data.Text.Utf16.Rope as Rope
import qualified Name
import qualified Position
import Protolude hiding (moduleName)
import Query (Query)
import qualified Query
import Rock
import qualified Scope
import Span (LineColumn (LineColumns))
import qualified Span

fromDefinitionName :: MonadFetch Query m => Scope.DefinitionKind -> Name.Qualified -> m (Maybe (Span.Relative -> Span.LineColumn))
fromDefinitionName definitionKind name@(Name.Qualified moduleName _) = do
  (_, maybeAbsolutePosition) <- fetch $ Query.DefinitionPosition definitionKind name
  toLineColumns <- fromAbsolute moduleName
  pure $ fmap ((toLineColumns .) . Span.absoluteFrom) maybeAbsolutePosition

fromAbsolute :: MonadFetch Query m => Name.Module -> m (Span.Absolute -> Span.LineColumn)
fromAbsolute moduleName = do
  maybeFilePath <- fetch $ Query.ModuleFile moduleName
  case maybeFilePath of
    Nothing ->
      pure $ const $ Span.LineColumns (Position.LineColumn 0 0) (Position.LineColumn 0 0)
    Just filePath -> do
      rope <- fetch $ Query.FileRope filePath
      let toLineColumn (Position.Absolute i) =
            case Rope.splitAt (fromIntegral i) rope of
              Nothing -> Position.LineColumn 0 0
              Just (rope', _) ->
                let Rope.Position row column = Rope.lengthAsPosition rope'
                 in Position.LineColumn (fromIntegral row) (fromIntegral column)

          toLineColumns (Span.Absolute start end) =
            Span.LineColumns (toLineColumn start) (toLineColumn end)

      return toLineColumns
