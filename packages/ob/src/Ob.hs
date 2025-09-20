{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note (..),
  Task (..),
  Vault (..),
  getVault,
  withLiveVault,
)
where

import Control.Monad.Logger (MonadLogger, runStdoutLoggingT)
import Data.Map.Strict qualified as Map
import Ob.Note (Note (..), parseNote)
import Ob.Task (Task (..))
import Ob.Vault (Vault (..), getTasks)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently_)

-- | Like `withVault` but returns the current snapshot, without monitoring it.
getVault :: FilePath -> IO Vault
getVault path = do
  runStdoutLoggingT $ do
    (notesMap, _) <- mountVault path
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
    pure $ Vault notesMap

{- | Calls `f` with a `TVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (TVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  runStdoutLoggingT $ do
    (notesMap0, modelF) <- mountVault path
    let initialVault = Vault notesMap0
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Map.size notesMap0)
    modelVar <- newTVarIO initialVault
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newNotesMap -> do
        let newVault = Vault newNotesMap
        let newTasks = getTasks newVault
        putTextLn $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
        atomically $ writeTVar modelVar newVault

mountVault ::
  (MonadUnliftIO m, MonadLogger m) =>
  FilePath ->
  m
    ( Map FilePath Note
    , (Map FilePath Note -> m ()) -> m ()
    )
mountVault path =
  UM.mount path (one ((), "**/*.md")) ["**/.*/**"] mempty (const $ handleMarkdownFile path)

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh _ _ -> do
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    pure $ Map.delete path
