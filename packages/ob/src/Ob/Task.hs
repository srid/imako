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
extractFromItem sourcePath [Plain (Str "☐" : Space : rest)] =
  [Task (extractText rest) sourcePath False]
extractFromItem sourcePath [Plain (Str "☑" : Space : rest)] =
  [Task (extractText rest) sourcePath True]
extractFromItem sourcePath [Plain (Str "[ ]" : Space : rest)] =
  [Task (extractText rest) sourcePath False]
extractFromItem sourcePath [Plain (Str "[x]" : Space : rest)] =
  [Task (extractText rest) sourcePath True]
extractFromItem _ _ = []

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
