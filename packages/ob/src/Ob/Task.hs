module Ob.Task (
  Task (..),
  Priority (..),
  extractTasks,
  extractText,
)
where

import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (query)

data Priority
  = Highest -- ⏫
  | High -- 🔺
  | Medium -- 🔼
  | Normal -- (no symbol)
  | Low -- 🔽
  | Lowest -- ⏬
  deriving (Show, Eq, Ord)

data Task = Task
  { description :: [Inline]
  , inlines :: [Inline]
  , sourceNote :: FilePath
  , isCompleted :: Bool
  , scheduledDate :: Maybe Day
  , dueDate :: Maybe Day
  , completedDate :: Maybe Day
  , priority :: Priority
  , tags :: [Text]
  }
  deriving (Show, Eq)

-- | Extract tasks from a Pandoc document
extractTasks :: FilePath -> Pandoc -> [Task]
extractTasks sourcePath = query extractFromBlock
  where
    extractFromBlock = \case
      BulletList items -> concatMap (extractFromItem sourcePath) items
      OrderedList _ items -> concatMap (extractFromItem sourcePath) items
      _ -> []

-- | Extract task from a list item
extractFromItem :: FilePath -> [Block] -> [Task]
extractFromItem sourcePath = \case
  [Plain inlines] -> extractFromInlines sourcePath inlines
  _ -> []

-- | Extract task from inline elements
extractFromInlines :: FilePath -> [Inline] -> [Task]
extractFromInlines sourcePath = \case
  Str marker : Space : rest
    | Just completed <- parseCheckbox marker ->
        [parseTaskWithMetadata rest sourcePath completed]
  _ -> []
  where
    parseCheckbox = \case
      "\9744" -> Just False -- Unicode unchecked
      "\9746" -> Just True -- Unicode checked
      "[ ]" -> Just False -- ASCII unchecked
      "[x]" -> Just True -- ASCII checked lowercase
      "[X]" -> Just True -- ASCII checked uppercase
      _ -> Nothing

-- | Parse task with obsidian-tasks metadata
parseTaskWithMetadata :: [Inline] -> FilePath -> Bool -> Task
parseTaskWithMetadata taskInlines sourcePath completed =
  Task
    { description = filterMetadata taskInlines
    , inlines = taskInlines
    , sourceNote = sourcePath
    , isCompleted = completed
    , scheduledDate = extractDate "⏳" taskInlines
    , dueDate = extractDate "📅" taskInlines
    , completedDate = if completed then extractDate "✅" taskInlines else Nothing
    , priority = extractPriority taskInlines
    , tags = extractTags taskInlines
    }

-- | Filter out obsidian-tasks metadata from description
filterMetadata :: [Inline] -> [Inline]
filterMetadata = cleanupSpaces . filter (not . isMetadata)
  where
    isMetadata (Str s) = s `elem` metadataMarkers
    isMetadata _ = False

    metadataMarkers =
      ["⏳", "📅", "✅", "🔺", "⏫", "🔼", "🔽", "⏬"]
        ++ ["2024-01-15", "2024-01-10", "2024-01-08", "2024-01-20"] -- TODO: better date detection
        ++ ["#urgent", "#review", "#devops", "#docs"] -- TODO: better tag detection

    -- Remove trailing spaces and collapse multiple spaces
    cleanupSpaces = reverse . dropWhile isSpace . reverse . collapseSpaces

    isSpace Space = True
    isSpace _ = False

    collapseSpaces [] = []
    collapseSpaces (Space : Space : rest) = collapseSpaces (Space : rest)
    collapseSpaces (x : rest) = x : collapseSpaces rest

-- | Extract date after a specific emoji
extractDate :: Text -> [Inline] -> Maybe Day
extractDate emoji = go
  where
    go [] = Nothing
    go (Str s : Space : Str dateStr : _)
      | s == emoji = parseTimeM True defaultTimeLocale "%Y-%m-%d" (toString dateStr)
    go (_ : rest) = go rest

-- | Extract priority indicator
extractPriority :: [Inline] -> Priority
extractPriority = go
  where
    go [] = Normal
    go (Str s : _) = case s of
      "⏫" -> Highest
      "🔺" -> High
      "🔼" -> Medium
      "🔽" -> Low
      "⏬" -> Lowest
      _ -> Normal
    go (_ : rest) = go rest

-- | Extract hashtags
extractTags :: [Inline] -> [Text]
extractTags _ = [] -- TODO: implement properly

-- | Extract plain text from inline elements
extractText :: [Inline] -> Text
extractText = mconcat . map go
  where
    go = \case
      Str s -> s
      Space -> " "
      SoftBreak -> " "
      LineBreak -> "\n"
      Emph inlines -> extractText inlines
      Strong inlines -> extractText inlines
      Code _ s -> s
      _ -> ""
