module Ob.Task (
  Task (..),
  extractTasks,
)
where

import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (query)

data Task = Task
  { taskText :: Text
  , sourceNote :: FilePath
  , isCompleted :: Bool
  }
  deriving (Show, Eq)

-- | Extract tasks from a Pandoc document
extractTasks :: FilePath -> Pandoc -> [Task]
extractTasks sourcePath = query extractFromBlock
  where
    extractFromBlock :: Block -> [Task]
    extractFromBlock (BulletList items) = concatMap (extractFromItem sourcePath) items
    extractFromBlock (OrderedList _ items) = concatMap (extractFromItem sourcePath) items
    extractFromBlock _ = []

-- | Extract task from a list item
extractFromItem :: FilePath -> [Block] -> [Task]
extractFromItem sourcePath [Plain inlines] = extractFromInlines sourcePath inlines
extractFromItem _ _ = []

-- | Extract task from inline elements
extractFromInlines :: FilePath -> [Inline] -> [Task]
-- Handle Unicode checkbox characters (what commonmark outputs)
extractFromInlines sourcePath (Str "\9744" : Space : rest) =
  [Task (extractText rest) sourcePath False]
extractFromInlines sourcePath (Str "\9746" : Space : rest) =
  [Task (extractText rest) sourcePath True]
-- Fallback patterns for other possible representations
extractFromInlines sourcePath (Str "[ ]" : Space : rest) =
  [Task (extractText rest) sourcePath False]
extractFromInlines sourcePath (Str "[x]" : Space : rest) =
  [Task (extractText rest) sourcePath True]
extractFromInlines sourcePath (Str "[X]" : Space : rest) =
  [Task (extractText rest) sourcePath True]
extractFromInlines _ _ = []

-- | Extract plain text from inline elements
extractText :: [Inline] -> Text
extractText = mconcat . map go
  where
    go (Str s) = s
    go Space = " "
    go SoftBreak = " "
    go LineBreak = "\n"
    go (Emph inlines) = extractText inlines
    go (Strong inlines) = extractText inlines
    go (Code _ s) = s
    go _ = ""
