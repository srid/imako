-- | Note parsing and data types for Obsidian notebooks
module Ob.Note (
  Note (..),
  parseNote,
)
where

import Data.Aeson qualified as Aeson
import Data.Time (UTCTime)
import Ob.Markdown (parseMarkdown)
import Ob.Task (Task, extractTasks)
import System.Directory (getModificationTime)
import Text.Pandoc.Definition (Pandoc)

data Note = Note
  { properties :: Maybe Aeson.Value
  , content :: Pandoc
  , tasks :: [Task]
  , modifiedAt :: UTCTime
  }

parseNote :: (MonadIO m) => FilePath -> m Note
parseNote path = do
  s <- decodeUtf8 <$> readFileBS path
  mtime <- liftIO $ getModificationTime path
  case parseMarkdown path s of
    Left err -> die $ "Error parsing " <> path <> ": " <> show err
    Right (meta, content) -> do
      let noteTasks = extractTasks path content
      pure $ Note meta content noteTasks mtime
