-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getTasks,
  getVault,
  withLiveVault,
)
where

import Effectful (Eff, IOE, withEffToIO, (:>))
import Effectful qualified
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (..), log, runLogActionStdout, withLogContext)
import Effectful.Internal.Monad (Limit (Unlimited), Persistence (Ephemeral), UnliftStrategy (ConcUnlift))
import Effectful.Reader.Static qualified as ER

import Colog (LogAction (..), WithSeverity (..))
import Colog.Message (RichMessage)
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
  Effectful.runEff . runLogActionStdout Info $ do
    Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
      (notesMap, _) <- mountVault path
      log Info $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
      pure $ Vault notesMap

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (LVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  Effectful.runEff . runLogActionStdout Info $ do
    Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
      (notesMap0, modelF) <- mountVault path
      let initialVault = Vault notesMap0
      log Info $ "Model ready; total docs = " <> show (Map.size notesMap0)
      modelVar <- LVar.new initialVault
      liftIO $ concurrently_ (f modelVar) $ do
        modelF $ \newNotesMap -> do
          Effectful.runEff . runLogActionStdout Info $ do
            Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
              let newVault = Vault newNotesMap
              let newTasks = getTasks newVault
              log Info $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
              liftIO $ LVar.set modelVar newVault

mountVault ::
  (ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) =>
  FilePath ->
  Eff
    es
    ( Map FilePath Note
    , (Map FilePath Note -> IO ()) -> IO ()
    )
mountVault path = withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO -> do
  let logger = LogAction $ \(WithSeverity msg sev) -> runInIO $ log sev msg
  (initial, umCallback) <- UM.mount logger path (one ((), "**/*.md")) ["**/.*/**"] mempty (const $ handleMarkdownFile path)
  let finalCallback userAction = do
        umCallback $ \notes -> liftIO $ userAction notes
  pure (initial, finalCallback)

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh _ _ -> do
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    pure $ Map.delete path
