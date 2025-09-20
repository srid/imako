module Ob.Task.Properties (
  Priority (..),
  TaskParseState (..),
  initialParseState,
  parseInlineSequence,
  parsePriority,
  parseTag,
  parseDateWithEmoji,
)
where

import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Text.Pandoc.Definition (Inline (..))

data Priority
  = Highest -- ⏫
  | High -- 🔺
  | Medium -- 🔼
  | Normal -- (no symbol)
  | Low -- 🔽
  | Lowest -- ⏬
  deriving (Show, Eq, Ord)

-- | State for collecting task properties while parsing
data TaskParseState = TaskParseState
  { cleanInlines :: [Inline]
  , tScheduledDate :: Maybe Day
  , tDueDate :: Maybe Day
  , tCompletedDate :: Maybe Day
  , tPriority :: Priority
  , tTags :: [Text]
  }

-- | Initial parsing state
initialParseState :: TaskParseState
initialParseState =
  TaskParseState
    { cleanInlines = []
    , tScheduledDate = Nothing
    , tDueDate = Nothing
    , tCompletedDate = Nothing
    , tPriority = Normal
    , tTags = []
    }

-- | Process inline elements, extracting metadata and building clean description
parseInlineSequence :: [Inline] -> TaskParseState
parseInlineSequence inlines =
  let result = go inlines initialParseState
   in result {cleanInlines = cleanupSpaces (reverse (cleanInlines result))}
  where
    go [] st = st
    go (Str s : Space : Str dateStr : rest) st =
      case parseDateWithEmoji s dateStr of
        Just (dateType, date) ->
          let newState = case dateType of
                "⏳" -> st {tScheduledDate = Just date}
                "📅" -> st {tDueDate = Just date}
                "✅" -> st {tCompletedDate = Just date}
                _ -> st
           in go rest newState
        Nothing -> go (Space : Str dateStr : rest) (processRegularInline (Str s) st)
    go (inline : rest) st = go rest (processRegularInline inline st)

    processRegularInline inline st = case inline of
      Str s -> case parsePriority s of
        Just p -> st {tPriority = p}
        Nothing -> case parseTag s of
          Just tag -> st {tTags = tag : tTags st}
          Nothing -> st {cleanInlines = inline : cleanInlines st}
      _ -> st {cleanInlines = inline : cleanInlines st}

-- | Parse priority emoji
parsePriority :: Text -> Maybe Priority
parsePriority = \case
  "⏫" -> Just Highest
  "🔺" -> Just High
  "🔼" -> Just Medium
  "🔽" -> Just Low
  "⏬" -> Just Lowest
  _ -> Nothing

-- | Parse hashtag
parseTag :: Text -> Maybe Text
parseTag s
  | "#" `T.isPrefixOf` s && T.length s > 1 = Just (T.drop 1 s)
  | otherwise = Nothing

-- | Parse date with emoji context
parseDateWithEmoji :: Text -> Text -> Maybe (Text, Day)
parseDateWithEmoji emoji dateStr = do
  date <- parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString dateStr)
  case emoji of
    "⏳" -> Just ("⏳", date)
    "📅" -> Just ("📅", date)
    "✅" -> Just ("✅", date)
    _ -> Nothing

-- | Clean up trailing spaces and collapse multiple spaces
cleanupSpaces :: [Inline] -> [Inline]
cleanupSpaces = reverse . dropWhile isSpace . reverse . collapseSpaces
  where
    isSpace Space = True
    isSpace _ = False

    collapseSpaces [] = []
    collapseSpaces (Space : Space : rest) = collapseSpaces (Space : rest)
    collapseSpaces (x : rest) = x : collapseSpaces rest
