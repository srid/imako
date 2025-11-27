-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getTasks,
  getVault,
  withLiveVault,
)
where

import Control.Monad.Logger (runStdoutLoggingT)
import Effectful
import Effectful.Colog.Simple (Severity (..), log, runLogActionStdout)

import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import Ob.Note (Note (..), parseNote)
import Ob.Task (Task)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import UnliftIO.Async (concurrently_)

-- | An Obsidian vault folder
newtype Vault = Vault
  { notes :: Map.Map FilePath Note
  }

getTasks :: Vault -> [Task]
getTasks vault = concatMap tasks (Map.elems vault.notes)

-- | Like `withVault` but returns the current snapshot, without monitoring it.
getVault :: FilePath -> IO Vault
getVault path = do
  runEff . runLogActionStdout Info $ do
    (notesMap, _) <- mountVault path
    log Info $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
    pure $ Vault notesMap

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (LVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  runEff . runLogActionStdout Info $ do
    (notesMap0, modelF) <- mountVault path
    let initialVault = Vault notesMap0
    log Info $ "Model ready; total docs = " <> show (Map.size notesMap0)
    modelVar <- LVar.new initialVault
    liftIO $ concurrently_ (f modelVar) $ do
      modelF $ \newNotesMap -> do
        runEff . runLogActionStdout Info $ do
          let newVault = Vault newNotesMap
          let newTasks = getTasks newVault
          log Info $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
          liftIO $ LVar.set modelVar newVault

mountVault ::
  (IOE :> es) =>
  FilePath ->
  Eff
    es
    ( Map FilePath Note
    , (Map FilePath Note -> IO ()) -> IO ()
    )
mountVault path = liftIO $ do
  (initial, umCallback) <- runStdoutLoggingT $ UM.mount path (one ((), "**/*.md")) ["**/.*/**"] mempty (const $ handleMarkdownFile path)
  let finalCallback userAction = runStdoutLoggingT $ do
        umCallback $ \notes -> liftIO $ userAction notes
  pure (initial, finalCallback)

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh _ _ -> do
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    pure $ Map.delete path
