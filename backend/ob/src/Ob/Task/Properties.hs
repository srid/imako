{-# LANGUAGE TemplateHaskell #-}

module Ob.Task.Properties (
  Priority (..),
  TaskProperties (..),
  initialTaskProperties,
  parseInlineSequence,
  parsePriority,
  parseTag,
  parseDateWithEmoji,
  priorityTsDeclarations,
)
where

import Data.Aeson (ToJSON (..), defaultOptions, object, (.=))
import Data.Aeson.TypeScript.Internal (TSDeclaration)
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.Text qualified as T
import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Ob.Task.Recurrence (Recurrence, parseRecurrence)
import Text.Pandoc.Definition (Inline (..))

data Priority
  = Highest -- ⏫
  | High -- 🔺
  | Medium -- 🔼
  | Normal -- (no symbol)
  | Low -- 🔽
  | Lowest -- ⏬
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''Priority)

-- | All TypeScript declarations from this module
priorityTsDeclarations :: [TSDeclaration]
priorityTsDeclarations = getTypeScriptDeclarations (Proxy @Priority)

-- | State for collecting task properties while parsing
data TaskProperties = TaskProperties
  { cleanInlines :: [Inline]
  , startDate :: Maybe Day
  , scheduledDate :: Maybe Day
  , dueDate :: Maybe Day
  , completedDate :: Maybe Day
  , priority :: Priority
  , tags :: [Text]
  , recurrence :: Maybe Recurrence
  }
  deriving stock (Show, Eq)

instance ToJSON TaskProperties where
  toJSON p =
    object
      [ "startDate" .= p.startDate
      , "scheduledDate" .= p.scheduledDate
      , "dueDate" .= p.dueDate
      , "completedDate" .= p.completedDate
      , "priority" .= p.priority
      , "tags" .= p.tags
      , "recurrence" .= p.recurrence
      ]

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
    , recurrence = Nothing
    }

-- | Process inline elements, extracting metadata and building clean description
parseInlineSequence :: [Inline] -> TaskProperties
parseInlineSequence inlines =
  let result = go inlines initialTaskProperties
   in result {cleanInlines = cleanupSpaces (reverse (cleanInlines result))}
  where
    go [] st = st
    -- Handle recurrence emoji
    go (Str "🔁" : Space : rest) st =
      let (recurrenceText, remaining) = extractRecurrenceText rest
          recur = parseRecurrence recurrenceText
       in go remaining (st {recurrence = recur})
    -- Handle date emojis with dates
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
    -- Handle all other inlines
    go (inline : rest) st = go rest (processRegularInline inline st)

    processRegularInline inline st = case inline of
      Str s -> case parsePriority s of
        Just p -> st {priority = p}
        Nothing -> case parseTag s of
          Just tag -> st {tags = tag : tags st}
          Nothing -> st {cleanInlines = inline : cleanInlines st}
      _ -> st {cleanInlines = inline : cleanInlines st}

    -- Extract recurrence text until we hit a date emoji or end
    extractRecurrenceText :: [Inline] -> (Text, [Inline])
    extractRecurrenceText inlines' =
      let (recParts, rest) = break isDateEmoji inlines'
          recText = unwords $ extractWords recParts
       in (recText, rest)

    isDateEmoji :: Inline -> Bool
    isDateEmoji (Str s) = s `elem` ["🛫", "⏳", "📅", "✅"]
    isDateEmoji _ = False

    extractWords :: [Inline] -> [Text]
    extractWords [] = []
    extractWords (Str s : rest') = s : extractWords rest'
    extractWords (Space : rest') = extractWords rest'
    extractWords (_ : rest') = extractWords rest'

-- | Parse priority emoji
parsePriority :: Text -> Maybe Priority
parsePriority = \case
  "🔺" -> Just Highest
  "⏫" -> Just High
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
