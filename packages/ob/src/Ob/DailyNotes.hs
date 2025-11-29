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

import Data.Aeson (FromJSON (..), (.!=), (.:?))
import Data.Aeson qualified as Aeson
import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import System.Directory (doesFileExist)
import System.FilePath (takeBaseName, takeDirectory, (</>))
import Text.Megaparsec (Parsec, anySingle, eof, parse, try)
import Text.Megaparsec.Char (string)
import Text.Pandoc.Definition (Pandoc)

-- | Configuration for Obsidian's daily notes plugin
data DailyNotesConfig = DailyNotesConfig
  { folder :: FilePath
  -- ^ Folder where daily notes are stored (relative to vault root). Defaults to ".".
  , format :: Text
  -- ^ Date format string (moment.js style, e.g., "YYYY-MM-DD"). Defaults to "YYYY-MM-DD".
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DailyNotesConfig where
  parseJSON = Aeson.withObject "DailyNotesConfig" $ \o ->
    DailyNotesConfig
      <$> o .:? "folder" .!= "."
      <*> o .:? "format" .!= "YYYY-MM-DD"

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

{- | Load daily notes configuration from the vault
Returns Nothing if the config file doesn't exist.
Throws an error if the config file exists but can't be parsed.
-}
loadDailyNotesConfig :: FilePath -> IO (Maybe DailyNotesConfig)
loadDailyNotesConfig vaultPath = do
  let configPath = vaultPath </> ".obsidian" </> "daily-notes.json"
  exists <- doesFileExist configPath
  if exists
    then do
      result <- Aeson.eitherDecodeFileStrict configPath
      case result of
        Left err -> error $ "Failed to parse " <> toText configPath <> ": " <> toText err
        Right config -> pure $ Just config
    else pure Nothing

-- | Check if a file path matches the daily notes pattern
isDailyNote :: DailyNotesConfig -> FilePath -> Bool
isDailyNote config path =
  let actualFolder = takeDirectory path
   in actualFolder == config.folder && isJust (parseDailyNoteDate config path)

-- | Parse the date from a daily note filename
parseDailyNoteDate :: DailyNotesConfig -> FilePath -> Maybe Day
parseDailyNoteDate config path =
  let baseName = takeBaseName path
      haskellFormat = momentToHaskellFormat config.format
   in parseTimeM True defaultTimeLocale haskellFormat baseName

-- | Convert moment.js date format to Haskell's time format
momentToHaskellFormat :: Text -> String
momentToHaskellFormat input =
  case parse formatParser "" input of
    Left _ -> "" -- Should not happen with this parser
    Right result -> result

type Parser = Parsec Void Text

formatParser :: Parser String
formatParser = concat <$> many formatToken <* eof

formatToken :: Parser String
formatToken =
  try (string "YYYY" $> "%Y")
    <|> try (string "YY" $> "%y")
    <|> try (string "MMMM" $> "%B")
    <|> try (string "MMM" $> "%b")
    <|> try (string "MM" $> "%m")
    <|> try (string "DD" $> "%d")
    <|> try (string "D" $> "%-d")
    <|> (pure <$> anySingle)

-- | Get the expected file path for today's daily note
getTodayNotePath :: DailyNotesConfig -> Day -> FilePath
getTodayNotePath config day =
  let haskellFormat = momentToHaskellFormat config.format
      filename = formatTime defaultTimeLocale haskellFormat day <> ".md"
   in config.folder </> filename
