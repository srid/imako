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
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import Ob.DailyNotes (DailyNote (..), DailyNotesConfig, loadDailyNotesConfig, mkDailyNote)
import Ob.Note (Note (..), parseNote)
import Ob.Task (Task)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import UnliftIO (MonadUnliftIO)
import UnliftIO.Async (concurrently_)

-- | An Obsidian vault folder
data Vault = Vault
  { notes :: Map.Map FilePath Note
  , dailyNotesConfig :: Maybe DailyNotesConfig
  -- ^ Configuration for daily notes plugin (if enabled)
  }

getTasks :: Vault -> [Task]
getTasks vault = concatMap tasks (Map.elems vault.notes)

-- | Get all daily notes from the vault, sorted by date (most recent first)
getDailyNotes :: Vault -> [DailyNote]
getDailyNotes vault = case vault.dailyNotesConfig of
  Nothing -> []
  Just config ->
    let matchingNotes =
          mapMaybe
            (\(path, note) -> mkDailyNote config path note.content)
            (Map.toList vault.notes)
     in sortOn (Down . (.day)) matchingNotes

-- | Like `withVault` but returns the current snapshot, without monitoring it.
getVault :: FilePath -> IO Vault
getVault path = do
  runStdoutLoggingT $ filterLogger (\_ level -> level >= LevelInfo) $ do
    (notesMap, _) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
    pure $ Vault notesMap dailyConfig

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: FilePath -> (LVar Vault -> IO ()) -> IO ()
withLiveVault path f = do
  runStdoutLoggingT $ filterLogger (\_ level -> level >= LevelInfo) $ do
    (notesMap0, modelF) <- mountVault path
    dailyConfig <- liftIO $ loadDailyNotesConfig path
    let initialVault = Vault notesMap0 dailyConfig
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Map.size notesMap0)
    modelVar <- LVar.new initialVault
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newNotesMap -> do
        let newVault = Vault newNotesMap dailyConfig
        let newTasks = getTasks newVault
        putTextLn $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
        LVar.set modelVar newVault

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
