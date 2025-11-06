-- | Module for working with obsidian-tasks
module Ob.Task (
  Task (..),
  TaskStatus (..),
  Priority (..),
  extractTasks,
  extractText,
)
where

import Ob.Task.Properties (Priority (..), TaskProperties (..), parseInlineSequence)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..))

data TaskStatus
  = Incomplete
  | InProgress
  | Cancelled
  | Completed
  deriving (Show, Eq)

data Task = Task
  { description :: [Inline]
  , inlines :: [Inline]
  , sourceNote :: FilePath
  , status :: TaskStatus
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
    | Just taskStatus <- parseCheckbox marker ->
        [parseTaskWithMetadata rest sourcePath parents taskStatus]
  _ -> []
  where
    parseCheckbox = \case
      "\9744" -> Just Incomplete -- Unicode unchecked
      "\9746" -> Just Completed -- Unicode checked
      "[ ]" -> Just Incomplete -- ASCII unchecked
      "[x]" -> Just Completed -- ASCII checked lowercase
      "[X]" -> Just Completed -- ASCII checked uppercase
      "[/]" -> Just InProgress -- In-progress
      "[-]" -> Just Cancelled -- Cancelled
      _ -> Nothing

-- | Parse task with obsidian-tasks metadata
parseTaskWithMetadata :: [Inline] -> FilePath -> [Text] -> TaskStatus -> Task
parseTaskWithMetadata taskInlines sourcePath parents taskStatus =
  let props = parseInlineSequence taskInlines
      isComplete = taskStatus == Completed
   in Task
        { description = cleanInlines props
        , inlines = taskInlines
        , sourceNote = sourcePath
        , status = taskStatus
        , properties =
            props
              { completedDate = if isComplete then completedDate props else Nothing
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
