{- | Daily notes configuration and types for Obsidian notebooks.

Reads configuration from .obsidian/daily-notes.json.
-}
module Ob.DailyNotes (
  DailyNotesConfig (..),
  DailyNote (..),
  loadDailyNotesConfig,
  isDailyNote,
  parseDailyNoteDate,
  getTodayNotePath,
  momentToHaskellFormat,
) where

import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Text.Pandoc.Definition (Pandoc)

-- | Configuration for Obsidian's daily notes plugin
data DailyNotesConfig = DailyNotesConfig
  { folder :: Maybe FilePath
  -- ^ Folder where daily notes are stored (relative to vault root)
  , format :: Text
  -- ^ Date format string (moment.js style, e.g., "YYYY-MM-DD")
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DailyNotesConfig where
  parseJSON = Aeson.withObject "DailyNotesConfig" $ \o ->
    DailyNotesConfig
      <$> o .:? "folder"
      <*> (o .: "format" <|> pure "YYYY-MM-DD")

-- | A daily note with its associated date and content
data DailyNote = DailyNote
  { day :: Day
  -- ^ The date this note represents
  , notePath :: FilePath
  -- ^ Path to the note file (relative to vault root)
  , noteContent :: Pandoc
  -- ^ The parsed content of the note
  }
  deriving stock (Show, Eq)

-- | Load daily notes configuration from the vault
loadDailyNotesConfig :: FilePath -> IO (Maybe DailyNotesConfig)
loadDailyNotesConfig vaultPath = do
  let configPath = vaultPath </> ".obsidian" </> "daily-notes.json"
  Aeson.decodeFileStrict configPath

-- | Check if a file path matches the daily notes pattern
isDailyNote :: DailyNotesConfig -> FilePath -> Bool
isDailyNote config path =
  let expectedFolder = fromMaybe "" config.folder
      actualFolder = takeDirectory path
      folderMatches = actualFolder == expectedFolder || (null expectedFolder && actualFolder == ".")
   in folderMatches && isJust (parseDailyNoteDate config path)

-- | Parse the date from a daily note filename
parseDailyNoteDate :: DailyNotesConfig -> FilePath -> Maybe Day
parseDailyNoteDate config path =
  let baseName = takeBaseName path
      haskellFormat = momentToHaskellFormat config.format
   in parseTimeM True defaultTimeLocale haskellFormat baseName

-- | Convert moment.js date format to Haskell's time format
momentToHaskellFormat :: Text -> String
momentToHaskellFormat = toString . go
  where
    go :: Text -> Text
    go t
      | "YYYY" `T.isPrefixOf` t = "%Y" <> go (T.drop 4 t)
      | "YY" `T.isPrefixOf` t = "%y" <> go (T.drop 2 t)
      | "MMMM" `T.isPrefixOf` t = "%B" <> go (T.drop 4 t)
      | "MMM" `T.isPrefixOf` t = "%b" <> go (T.drop 3 t)
      | "MM" `T.isPrefixOf` t = "%m" <> go (T.drop 2 t)
      | "DD" `T.isPrefixOf` t = "%d" <> go (T.drop 2 t)
      | "D" `T.isPrefixOf` t = "%-d" <> go (T.drop 1 t)
      | otherwise = case T.uncons t of
          Nothing -> ""
          Just (c, rest) -> one c <> go rest

-- | Get the expected file path for today's daily note
getTodayNotePath :: DailyNotesConfig -> Day -> FilePath
getTodayNotePath config day =
  let haskellFormat = momentToHaskellFormat config.format
      filename = formatTime defaultTimeLocale haskellFormat day <> ".md"
   in case config.folder of
        Nothing -> filename
        Just folder -> folder </> filename
