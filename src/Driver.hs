{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Driver where

import Control.Concurrent.Async.Lifted.Safe
import Control.Concurrent.Lifted
import Control.Monad.Trans.Control
import Data.Constraint.Extras (has')
import Data.Dependent.HashMap (DHashMap)
import qualified Data.Dependent.HashMap as DHashMap
import Data.Dependent.Sum (DSum ((:=>)))
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.IORef.Lifted
import Data.Persist (Persist)
import qualified Data.Persist as Persist
import qualified Data.Text.IO as Text
import Data.Text.Utf16.Rope (Rope)
import qualified Data.Text.Utf16.Rope as Rope
import Error (Error)
import qualified Error.Hydrated
import qualified Error.Hydrated as Error (Hydrated)
import qualified FileSystem
import qualified Name
import qualified Paths_sixty as Paths
import Protolude hiding (State, getNumCapabilities, state)
import Query (Query)
import qualified Query
import Rock
import qualified Rules

runTask ::
  HashSet FileSystem.Directory ->
  HashSet FilePath ->
  (Error.Hydrated -> Task Query err) ->
  Task Query a ->
  IO (a, [err])
runTask sourceDirectories files prettyError task = do
  startedVar <- newIORef mempty
  errorsVar <- newIORef (mempty :: DHashMap Query (Const [Error]))
  -- printVar <- newMVar 0
  threadDepsVar <- newIORef mempty
  let writeErrors :: Writer TaskKind Query a -> [Error] -> Task Query ()
      writeErrors (Writer q) errs =
        unless (null errs) $
          atomicModifyIORef' errorsVar $
            (,()) . DHashMap.insert q (Const errs)

      ignoreTaskKind :: Query a -> TaskKind -> Task Query ()
      ignoreTaskKind _ _ =
        pure ()

      --     traceFetch_
      --       :: GenRules (Writer TaskKind Query) Query
      --       -> GenRules (Writer TaskKind Query) Query
      --     traceFetch_ =
      --       traceFetch
      --         (\(Writer key) -> modifyMVar_ printVar $ \n -> do
      --           putText $ fold (replicate n "| ") <> "fetching " <> show key
      --           return $ n + 1)
      --         (\_ _ -> modifyMVar_ printVar $ \n -> do
      --           putText $ fold (replicate (n - 1) "| ") <> "*"
      --           return $ n - 1)

      rules :: Rules Query
      rules =
        memoiseWithCycleDetection startedVar threadDepsVar $
          writer ignoreTaskKind $
            -- traceFetch_ $
            writer writeErrors $
              Rules.rules sourceDirectories files $ \file ->
                Right <$> readFile file `catch` \(_ :: IOException) -> pure mempty

  Rock.runTask rules $ do
    -- Rock.runMemoisedTask startedVar rules $ do
    result <- task
    errorsMap <- readIORef errorsVar
    let errors =
          flip foldMap (DHashMap.toList errorsMap) $ \(_ :=> Const errs) ->
            errs
    prettyErrors <- forM errors (prettyError <=< Error.Hydrated.fromError)
    pure (result, prettyErrors)

-------------------------------------------------------------------------------
-- Incremental execution
data State err = State
  { _startedVar :: !(IORef (DHashMap Query MemoEntry))
  , _hashesVar :: !(IORef (DHashMap Query (Const Int)))
  , _reverseDependenciesVar :: !(IORef (ReverseDependencies Query))
  , _tracesVar :: !(IORef (Traces Query (Const Int)))
  , _errorsVar :: !(IORef (DHashMap Query (Const [err])))
  }

initialState :: IO (State err)
initialState = do
  startedVar <- newIORef mempty
  hashesVar <- newIORef mempty
  reverseDependenciesVar <- newIORef mempty
  tracesVar <- newIORef mempty
  errorsVar <- newIORef mempty
  return
    State
      { _startedVar = startedVar
      , _hashesVar = hashesVar
      , _reverseDependenciesVar = reverseDependenciesVar
      , _tracesVar = tracesVar
      , _errorsVar = errorsVar
      }

encodeState :: Persist err => State (err, doc) -> IO ByteString
encodeState state = do
  traces <- readIORef $ _tracesVar state
  errors <- readIORef $ _errorsVar state
  pure $
    Persist.encode (traces, DHashMap.map (\(Const errDocs) -> Const $ fst <$> errDocs) errors)

decodeState :: Persist err => ByteString -> IO (State err)
decodeState bs = do
  s <- initialState
  case Persist.decode bs of
    Right (traces, errors) -> do
      void $ atomicWriteIORef (_tracesVar s) traces
      void $ atomicWriteIORef (_errorsVar s) errors
    Left _ ->
      pure ()
  pure s

data Prune
  = Don'tPrune
  | Prune

runIncrementalTask ::
  State err ->
  HashSet FilePath ->
  HashSet FilePath ->
  HashMap FilePath (Either Rope Text) ->
  (Error.Hydrated -> Task Query err) ->
  Prune ->
  Task Query a ->
  IO (a, [err])
runIncrementalTask state changedFiles sourceDirectories files prettyError prune task =
  handleEx $ do
    do
      reverseDependencies <- readIORef $ _reverseDependenciesVar state
      started <- readIORef $ _startedVar state
      hashes <- readIORef $ _hashesVar state

      case DHashMap.lookup Query.InputFiles started of
        Just (Done inputFiles) -> do
          -- TODO find a nicer way to do this
          builtinFile <- Paths.getDataFileName "builtin/Builtin.vix"
          if inputFiles /= HashSet.insert builtinFile (HashSet.fromMap $ void files)
            then do
              atomicWriteIORef (_reverseDependenciesVar state) mempty
              atomicWriteIORef (_startedVar state) mempty
              atomicWriteIORef (_hashesVar state) mempty
            else do
              changedFiles' <- flip filterM (toList changedFiles) $ \file ->
                pure $ case (HashMap.lookup file files, DHashMap.lookup (Query.FileRope file) started, DHashMap.lookup (Query.FileText file) started) of
                  (Just (Left rope), Just (Done rope'), _) -> rope /= rope'
                  (Just (Left rope), _, Just (Done text')) -> Rope.toText rope /= text'
                  (Just (Right text), _, Just (Done text')) -> text /= text'
                  (Just (Right text), Just (Done rope'), _) -> text /= Rope.toText rope'
                  _ -> True
              let (keysToInvalidate, reverseDependencies') =
                    foldl'
                      ( \(keysToInvalidate_, reverseDependencies_) file ->
                          first (<> keysToInvalidate_) $ reachableReverseDependencies (Query.FileText file) reverseDependencies_
                      )
                      (mempty, reverseDependencies)
                      changedFiles'
              let started' =
                    DHashMap.difference started keysToInvalidate

                  hashes' =
                    DHashMap.difference hashes keysToInvalidate
              -- Text.hPutStrLn stderr $ "keysToInvalidate " <> show (DHashMap.size keysToInvalidate)
              -- Text.hPutStrLn stderr $ "Started " <> show (DHashMap.size started) <> " -> " <> show (DHashMap.size started')
              -- Text.hPutStrLn stderr $ "Hashes " <> show (DHashMap.size hashes) <> " -> " <> show (DHashMap.size hashes')
              -- Text.hPutStrLn stderr $ "ReverseDependencies " <> show (Map.size reverseDependencies) <> " -> " <> show (Map.size reverseDependencies')

              atomicWriteIORef (_startedVar state) started'
              atomicWriteIORef (_hashesVar state) hashes'
              atomicWriteIORef (_reverseDependenciesVar state) reverseDependencies'

        -- printVar <- newMVar 0
        _ -> do
          atomicWriteIORef (_reverseDependenciesVar state) mempty
          atomicWriteIORef (_startedVar state) mempty
          atomicWriteIORef (_hashesVar state) mempty

    threadDepsVar <- newIORef mempty
    let readSourceFile_ file
          | Just contents <- HashMap.lookup file files =
            return contents
          | otherwise =
            Right <$> readFile file `catch` \(_ :: IOException) -> pure mempty

        traceFetch_ ::
          GenRules (Writer TaskKind Query) Query ->
          GenRules (Writer TaskKind Query) Query
        traceFetch_ r = r
        -- traceFetch_ =
        --   traceFetch
        --     (\(Writer key) -> modifyMVar_ printVar $ \n -> do
        --       putText $ fold (replicate n "| ") <> "fetching " <> show key
        --       return $ n + 1)
        --     (\_ _ -> modifyMVar_ printVar $ \n -> do
        --       putText $ fold (replicate (n - 1) "| ") <> "*"
        --       return $ n - 1)
        writeErrors :: Writer TaskKind Query a -> [Error] -> Task Query ()
        writeErrors (Writer key) errs = do
          errs' <- mapM (prettyError <=< Error.Hydrated.fromError) errs
          atomicModifyIORef' (_errorsVar state) $
            (,()) . if null errs' then DHashMap.delete key else DHashMap.insert key (Const errs')

        rules :: Rules Query
        rules =
          memoiseWithCycleDetection (_startedVar state) threadDepsVar $
            trackReverseDependencies (_reverseDependenciesVar state) $
              verifyTraces
                (_tracesVar state)
                ( \query value -> do
                    hashed <- readIORef $ _hashesVar state
                    case DHashMap.lookup query hashed of
                      Just h ->
                        pure h
                      Nothing -> do
                        let h =
                              Const $ has' @Hashable @Identity query $ hash $ Identity value
                        atomicModifyIORef' (_hashesVar state) $
                          (,()) . DHashMap.insert query h
                        pure h
                )
                $ traceFetch_ $
                  writer writeErrors $
                    Rules.rules sourceDirectories (HashSet.fromMap $ void files) readSourceFile_
    -- result <- Rock.runMemoisedTask (_startedVar state) rules task
    result <- Rock.runTask rules task
    started <- readIORef $ _startedVar state
    errorsMap <- case prune of
      Don'tPrune ->
        readIORef $ _errorsVar state
      Prune -> do
        atomicModifyIORef' (_tracesVar state) $
          (,()) . DHashMap.intersectionWithKey (\_ _ t -> t) started
        atomicModifyIORef' (_errorsVar state) $ \errors -> do
          let errors' = DHashMap.intersectionWithKey (\_ _ e -> e) started errors
          (errors', errors')
    let errors = do
          (_ :=> Const errs) <- DHashMap.toList errorsMap
          errs
    pure (result, errors)
  where
    handleEx m =
      m `catch` \e -> do
        Text.hPutStrLn stderr $ "exception! " <> show (e :: SomeException)
        panic $ show e

-------------------------------------------------------------------------------

checkAll :: Task Query ()
checkAll = do
  filePaths <- fetch Query.InputFiles
  forConcurrently_ filePaths $ \filePath -> do
    (module_, _, defs) <- fetch $ Query.ParsedFile filePath
    let names =
          HashSet.fromList $
            Name.Qualified module_ . fst . snd <$> defs
    forM_ (HashSet.toList names) $ \name -> do
      void $ fetch $ Query.ElaboratedType name
      fetch $ Query.ElaboratedDefinition name

pooledForConcurrently_ ::
  (Foldable t, MonadBaseControl IO m) =>
  t a ->
  (a -> m b) ->
  m ()
pooledForConcurrently_ as f =
  liftBaseWith $ \runInIO ->
    pooledForConcurrentlyIO_ as (runInIO . f)

pooledForConcurrentlyIO_ ::
  Foldable t =>
  t a ->
  (a -> IO b) ->
  IO ()
pooledForConcurrentlyIO_ as f = do
  todoRef <- newIORef $ toList as
  processCount <- getNumCapabilities
  let go =
        join $
          atomicModifyIORef' todoRef $ \todo ->
            case todo of
              [] ->
                (todo, pure ())
              (a : todo') ->
                ( todo'
                , do
                    _ <- f a
                    go
                )
  replicateConcurrently_ (max 8 processCount) go

pooledForConcurrentlyIO ::
  Traversable t =>
  t a ->
  (a -> IO b) ->
  IO (t b)
pooledForConcurrentlyIO as f = do
  jobs <- forM as $ \a -> do
    ref <- newIORef $ panic "pooledForConcurrently not done"
    pure (a, ref)
  todoRef <- newIORef $ toList jobs
  processCount <- getNumCapabilities
  let go =
        join $
          atomicModifyIORef' todoRef $ \todo ->
            case todo of
              [] ->
                (todo, pure ())
              ((a, ref) : todo') ->
                ( todo'
                , do
                    result <- f a
                    atomicWriteIORef ref result
                    go
                )
  replicateConcurrently_ (max 8 processCount) go
  forM jobs $ \(_, ref) ->
    readIORef ref

pooledForConcurrently ::
  (Traversable t, MonadBaseControl IO m, StM m b ~ b) =>
  t a ->
  (a -> m b) ->
  m (t b)
pooledForConcurrently as f =
  liftBaseWith $ \runInIO ->
    pooledForConcurrentlyIO as (runInIO . f)
