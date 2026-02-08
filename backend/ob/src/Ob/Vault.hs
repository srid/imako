-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getVault,
  withLiveVault,
)
where

import Control.Monad.Logger (LogLevel (..), MonadLogger, filterLogger, runStdoutLoggingT)
import Data.IxSet.Typed qualified as Ix
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Ob.DailyNotes (DailyNote (..), DailyNotesConfig, loadDailyNotesConfig, mkDailyNote)
import Ob.Note (IxNote, Note (..), parseNote)
import Ob.Task (IxTask, noteTasks)
import Ob.Vault.Ix (deleteIxMulti, updateIxMulti)
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently_)

-- | An Obsidian vault folder
data Vault = Vault
  { notes :: IxNote
  , tasks :: IxTask
  , dailyNotesConfig :: Maybe DailyNotesConfig
  -- ^ Configuration for daily notes plugin (if enabled)
  }

-- | Get all daily notes from the vault, sorted by date (most recent first)
getDailyNotes :: Vault -> [DailyNote]
getDailyNotes vault = case vault.dailyNotesConfig of
  Nothing -> []
  Just config ->
    let matchingNotes =
          mapMaybe
            (\note -> mkDailyNote config note.path note.content)
            (Ix.toList vault.notes)
     in sortOn (Down . (.day)) matchingNotes

-- | Like `withVault` but returns the current snapshot, without monitoring it.
getVault :: FilePath -> IO Vault
getVault path = do
  runStdoutLoggingT $ filterLogger (\_ level -> level >= LevelInfo) $ do
    ((notes0, tasks0), _) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Ix.size notes0) <> "; sample = " <> show (take 4 $ map (.path) $ Ix.toList notes0)
    pure $ Vault notes0 tasks0 dailyConfig

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (LVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  runStdoutLoggingT $ filterLogger (\_ level -> level >= LevelInfo) $ do
    ((notes0, tasks0), modelF) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    let initialVault = Vault notes0 tasks0 dailyConfig
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Ix.size notes0)
    modelVar <- LVar.new initialVault
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \(newNotes, newTasks) -> do
        let newVault = Vault newNotes newTasks dailyConfig
        putTextLn $ "Model updated; total docs = " <> show (Ix.size newNotes) <> "; total tasks = " <> show (Ix.size newTasks)
        LVar.set modelVar newVault

mountVault ::
  (MonadUnliftIO m, MonadLogger m) =>
  FilePath ->
  m
    ( (IxNote, IxTask)
    , ((IxNote, IxTask) -> m ()) -> m ()
    )
mountVault path =
  UM.mount path (one ((), "**/*.md")) ["**/.*/**"] (Ix.empty, Ix.empty) (const $ handleMarkdownFile path)

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m ((IxNote, IxTask) -> (IxNote, IxTask))
handleMarkdownFile baseDir relativePath = \case
  UM.Refresh _ _ -> do
    note <- parseNote baseDir relativePath
    let newTasks = noteTasks note.path note.content
    pure $
      bimap
        (Ix.updateIx relativePath note)
        (updateIxMulti relativePath newTasks)
  UM.Delete -> do
    pure $
      bimap
        (Ix.deleteIx relativePath)
        (deleteIxMulti relativePath)
