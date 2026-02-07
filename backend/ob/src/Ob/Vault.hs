-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getTasks,
  getDailyNotes,
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
import Ob.Task (Task)
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently_)

-- | An Obsidian vault folder
data Vault = Vault
  { notes :: IxNote
  , dailyNotesConfig :: Maybe DailyNotesConfig
  -- ^ Configuration for daily notes plugin (if enabled)
  }

-- | Collect all tasks from every note in the vault.
getTasks :: Vault -> [Task]
getTasks vault = concatMap (.tasks) (Ix.toList vault.notes)

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
    (notes0, _) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Ix.size notes0) <> "; sample = " <> show (take 4 $ map (.path) $ Ix.toList notes0)
    pure $ Vault notes0 dailyConfig

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (LVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  runStdoutLoggingT $ filterLogger (\_ level -> level >= LevelInfo) $ do
    (notes0, modelF) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    let initialVault = Vault notes0 dailyConfig
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Ix.size notes0)
    modelVar <- LVar.new initialVault
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newNotes -> do
        let newVault = Vault newNotes dailyConfig
        let newTasks = getTasks newVault
        putTextLn $ "Model updated; total docs = " <> show (Ix.size newNotes) <> "; total tasks = " <> show (length newTasks)
        LVar.set modelVar newVault

mountVault ::
  (MonadUnliftIO m, MonadLogger m) =>
  FilePath ->
  m
    ( IxNote
    , (IxNote -> m ()) -> m ()
    )
mountVault path =
  UM.mount path (one ((), "**/*.md")) ["**/.*/**"] Ix.empty (const $ handleMarkdownFile path)

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (IxNote -> IxNote)
handleMarkdownFile baseDir relativePath = \case
  UM.Refresh _ _ -> do
    note <- parseNote baseDir relativePath
    pure $ Ix.updateIx relativePath note
  UM.Delete -> do
    pure $ Ix.deleteIx relativePath
