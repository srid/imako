module Ob.Task.Properties (
  Priority (..),
  TaskProperties (..),
  initialTaskProperties,
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
data TaskProperties = TaskProperties
  { cleanInlines :: [Inline]
  , startDate :: Maybe Day
  , scheduledDate :: Maybe Day
  , dueDate :: Maybe Day
  , completedDate :: Maybe Day
  , priority :: Priority
  , tags :: [Text]
  }
  deriving (Show, Eq)

-- | Initial task properties
initialTaskProperties :: TaskProperties
initialTaskProperties =
  TaskProperties
    { cleanInlines = []
    , startDate = Nothing
    , scheduledDate = Nothing
    , dueDate = Nothing
    , completedDate = Nothing
    , priority = Normal
    , tags = []
    }

-- | Process inline elements, extracting metadata and building clean description
parseInlineSequence :: [Inline] -> TaskProperties
parseInlineSequence inlines =
  let result = go inlines initialTaskProperties
   in result {cleanInlines = cleanupSpaces (reverse (cleanInlines result))}
  where
    go [] st = st
    go (Str s : Space : Str dateStr : rest) st =
      case parseDateWithEmoji s dateStr of
        Just (dateType, date) ->
          let newState = case dateType of
                "🛫" -> st {startDate = Just date}
                "⏳" -> st {scheduledDate = Just date}
                "📅" -> st {dueDate = Just date}
                "✅" -> st {completedDate = Just date}
                _ -> st
           in go rest newState
        Nothing -> go (Space : Str dateStr : rest) (processRegularInline (Str s) st)
    go (inline : rest) st = go rest (processRegularInline inline st)

    -- | Process a regular inline element that's not part of a date sequence
    processRegularInline :: Inline -> TaskProperties -> TaskProperties
    processRegularInline inline st = case inline of
      Str s -> case parsePriority s of
        Just p -> st {priority = p}
        Nothing -> case parseTag s of
          Just tag -> st {tags = tag : tags st}
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
    "🛫" -> Just ("🛫", date)
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
