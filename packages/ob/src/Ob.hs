{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note (..),
  Task (..),
  Notebook (..),
  getNotebook,
  withLiveNotebook,
)
where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Ob.Markdown (parseMarkdown)
import Ob.Task (Task (..), extractTasks)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import Text.Pandoc.Definition (Pandoc)
import UnliftIO.Async (concurrently_)

data Note = Note
  { properties :: Maybe Aeson.Value
  , content :: Pandoc
  }

data Notebook = Notebook
  { notes :: Map FilePath Note
  , tasks :: [Task]
  }

-- | Like `withNotebook` but returns the current snapshot, without monitoring it.
getNotebook :: FilePath -> IO Notebook
getNotebook path = do
  runStdoutLoggingT $ do
    (notesMap, _) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handleMarkdownFile path)
    let allTasks = concatMap (\(filePath, note) -> extractTasks filePath (content note)) (Map.toList notesMap)
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
    pure $ Notebook notesMap allTasks

{- | Calls `f` with a `TVar` of `Notebook` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveNotebook :: FilePath -> (TVar Notebook -> IO ()) -> IO ()
withLiveNotebook path f = do
  runStdoutLoggingT $ do
    (notesMap0, modelF) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handleMarkdownFile path)
    let initialTasks = concatMap (\(filePath, note) -> extractTasks filePath (content note)) (Map.toList notesMap0)
    let initialNotebook = Notebook notesMap0 initialTasks
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Map.size notesMap0)
    modelVar <- newTVarIO initialNotebook
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newNotesMap -> do
        -- FIXME: optimize.
        let newTasks = concatMap (\(filePath, note) -> extractTasks filePath (content note)) (Map.toList newNotesMap)
        let newNotebook = Notebook newNotesMap newTasks
        putTextLn $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
        atomically $ writeTVar modelVar newNotebook

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh _ _ -> do
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    pure $ Map.delete path

parseNote :: (MonadIO m) => FilePath -> m Note
parseNote path = do
  s <- decodeUtf8 <$> readFileBS path
  case parseMarkdown path s of
    Left err -> die $ show err
    Right (meta, content) -> pure $ Note meta content
