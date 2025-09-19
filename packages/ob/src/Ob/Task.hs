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
  -- Handle Unicode checkbox characters (what commonmark outputs)
  Str "\9744" : Space : rest -> [Task rest sourcePath False]
  Str "\9746" : Space : rest -> [Task rest sourcePath True]
  -- Fallback patterns for other possible representations
  Str "[ ]" : Space : rest -> [Task rest sourcePath False]
  Str "[x]" : Space : rest -> [Task rest sourcePath True]
  Str "[X]" : Space : rest -> [Task rest sourcePath True]
  _ -> []

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
