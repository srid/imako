module Ob.Task (
  Task (..),
  extractTasks,
  extractText,
)
where

import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc)
import Text.Pandoc.Walk (query)

data Task = Task
  { description :: [Inline]
  , sourceNote :: FilePath
  , isCompleted :: Bool
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
        [Task rest sourcePath completed]
  _ -> []
  where
    parseCheckbox = \case
      "\9744" -> Just False -- Unicode unchecked
      "\9746" -> Just True -- Unicode checked
      "[ ]" -> Just False -- ASCII unchecked
      "[x]" -> Just True -- ASCII checked lowercase
      "[X]" -> Just True -- ASCII checked uppercase
      _ -> Nothing

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
