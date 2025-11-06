-- | Module for working with obsidian-tasks
module Ob.Task (
  Task (..),
  Priority (..),
  extractTasks,
  extractText,
)
where

import Ob.Task.Properties (Priority (..), TaskProperties (..), parseInlineSequence)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))

data Task = Task
  { description :: [Inline]
  , inlines :: [Inline]
  , sourceNote :: FilePath
  , isCompleted :: Bool
  , properties :: TaskProperties
  , parentTasks :: [Text]
  }
  deriving (Show, Eq)

-- | Extract tasks from a Pandoc document
extractTasks :: FilePath -> Pandoc -> [Task]
extractTasks sourcePath (Pandoc _ blocks) = concatMap (extractFromBlock sourcePath []) blocks

-- | Extract tasks from a block, tracking parent task context
extractFromBlock :: FilePath -> [Text] -> Block -> [Task]
extractFromBlock path parents = \case
  BulletList items -> concatMap (extractFromItem path parents) items
  OrderedList _ items -> concatMap (extractFromItem path parents) items
  _ -> []

-- | Extract task from a list item, tracking parent tasks
extractFromItem :: FilePath -> [Text] -> [Block] -> [Task]
extractFromItem sourcePath parents blocks =
  let (maybeTask, nestedBlocks) = extractTaskFromBlocks blocks
      -- If this item is a task, add it to the parent chain for nested items
      newParents = case maybeTask of
        Just task -> parents <> [extractText task.description]
        Nothing -> parents
      -- Extract tasks from nested lists
      nestedTasks = concatMap (extractFromBlock sourcePath newParents) nestedBlocks
   in -- Return this task (if any) followed by nested tasks
      maybeToList maybeTask <> nestedTasks
  where
    -- Split blocks into task info and nested blocks
    extractTaskFromBlocks :: [Block] -> (Maybe Task, [Block])
    extractTaskFromBlocks = \case
      Plain inlines : rest ->
        let task = listToMaybe (extractFromInlines sourcePath parents inlines)
         in (task, rest)
      _ -> (Nothing, blocks)

-- | Extract task from inline elements
extractFromInlines :: FilePath -> [Text] -> [Inline] -> [Task]
extractFromInlines sourcePath parents = \case
  Str marker : Space : rest
    | Just completed <- parseCheckbox marker ->
        [parseTaskWithMetadata rest sourcePath parents completed]
  _ -> []
  where
    parseCheckbox = \case
      "\9744" -> Just False -- Unicode unchecked
      "\9746" -> Just True -- Unicode checked
      "[ ]" -> Just False -- ASCII unchecked
      "[x]" -> Just True -- ASCII checked lowercase
      "[X]" -> Just True -- ASCII checked uppercase
      "[/]" -> Just False -- In-progress (treat as incomplete)
      "[-]" -> Just True -- Cancelled (treat as complete)
      "[>]" -> Just False -- Forwarded/deferred (treat as incomplete)
      "[<]" -> Just False -- Scheduling (treat as incomplete)
      "[?]" -> Just False -- Question (treat as incomplete)
      "[!]" -> Just False -- Important (treat as incomplete)
      "[*]" -> Just False -- Star (treat as incomplete)
      "[n]" -> Just False -- Note (treat as incomplete)
      "[l]" -> Just False -- Location (treat as incomplete)
      "[i]" -> Just False -- Info (treat as incomplete)
      "[I]" -> Just False -- Idea (treat as incomplete)
      "[S]" -> Just False -- Amount/sum (treat as incomplete)
      "[p]" -> Just False -- Pro (treat as incomplete)
      "[c]" -> Just False -- Con (treat as incomplete)
      "[b]" -> Just False -- Bookmark (treat as incomplete)
      _ -> Nothing

-- | Parse task with obsidian-tasks metadata
parseTaskWithMetadata :: [Inline] -> FilePath -> [Text] -> Bool -> Task
parseTaskWithMetadata taskInlines sourcePath parents completed =
  let props = parseInlineSequence taskInlines
   in Task
        { description = cleanInlines props
        , inlines = taskInlines
        , sourceNote = sourcePath
        , isCompleted = completed
        , properties =
            props
              { completedDate = if completed then completedDate props else Nothing
              , tags = reverse (tags props)
              }
        , parentTasks = parents
        }

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
      Link _ inlines (url, _) ->
        let linkText = extractText inlines
         in if linkText == "" then url else linkText
      _ -> ""
