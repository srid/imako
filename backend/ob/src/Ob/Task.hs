-- | Module for working with obsidian-tasks
module Ob.Task (
  Task (..),
  TaskStatus (..),
  Priority (..),
  TaskProperties (..),
  extractTasks,
  extractText,
  renderInlines,
)
where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Time (Day)
import Lucid
import Ob.Task.Properties (Priority (..), TaskProperties (..), parseInlineSequence)
import Text.Pandoc.Definition (Block (..), Inline (..), Pandoc (..), QuoteType (..))

-- | Status of a task in obsidian-tasks format
data TaskStatus
  = -- | [ ] Not yet started
    Incomplete
  | -- | [/] Currently being worked on
    InProgress
  | -- | [-] Cancelled/abandoned
    Cancelled
  | -- | [x] Done
    Completed
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

{- | Parent task context containing task hierarchy information.

Each element is a tuple of:

* Task description text (for displaying breadcrumb trails)
* Optional start date (for filtering future task subtrees)

The list represents the full ancestor chain from root to immediate parent.
-}
type ParentContext = [(Text, Maybe Day)]

-- | Represents a task extracted from an Obsidian markdown file
data Task = Task
  { description :: [Inline]
  -- ^ Task description with metadata (dates, tags, priority) removed
  , inlines :: [Inline]
  -- ^ Original inline elements including all metadata
  , sourceNote :: FilePath
  -- ^ Path to the markdown file containing this task
  , status :: TaskStatus
  -- ^ Current status of the task
  , properties :: TaskProperties
  -- ^ Parsed metadata: dates, priority, tags, etc.
  , parentContext :: ParentContext
  -- ^ Parent task hierarchy: descriptions (for breadcrumbs) and start dates (for filtering)
  }
  deriving stock (Show, Eq)

-- | Custom ToJSON instance that extracts text from Pandoc inlines
instance ToJSON Task where
  toJSON task =
    object
      [ "description" .= extractText task.description
      , "sourceNote" .= task.sourceNote
      , "status" .= task.status
      , "dueDate" .= task.properties.dueDate
      , "scheduledDate" .= task.properties.scheduledDate
      , "startDate" .= task.properties.startDate
      , "completedDate" .= task.properties.completedDate
      , "priority" .= task.properties.priority
      , "tags" .= task.properties.tags
      , "parentBreadcrumbs" .= map fst task.parentContext
      ]

-- | Extract tasks from a Pandoc document
extractTasks :: FilePath -> Pandoc -> [Task]
extractTasks sourcePath (Pandoc _ blocks) = concatMap (extractFromBlock sourcePath []) blocks

-- | Extract tasks from a block, tracking parent task context
extractFromBlock :: FilePath -> ParentContext -> Block -> [Task]
extractFromBlock path parents = \case
  BulletList items -> concatMap (extractFromItem path parents) items
  OrderedList _ items -> concatMap (extractFromItem path parents) items
  _ -> []

-- | Extract task from a list item, tracking parent tasks
extractFromItem :: FilePath -> ParentContext -> [Block] -> [Task]
extractFromItem sourcePath parents blocks =
  let (maybeTask, nestedBlocks) = extractTaskFromBlocks blocks
      -- If this item is a task, add it to the parent chain for nested items
      newParents = case maybeTask of
        Just task -> parents <> [(extractText task.description, task.properties.startDate)]
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
      Para inlines : rest ->
        let task = listToMaybe (extractFromInlines sourcePath parents inlines)
         in (task, rest)
      _ -> (Nothing, blocks)

-- | Extract task from inline elements
extractFromInlines :: FilePath -> ParentContext -> [Inline] -> [Task]
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
parseTaskWithMetadata :: [Inline] -> FilePath -> ParentContext -> TaskStatus -> Task
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
        , parentContext = parents
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
      Underline inlines -> extractText inlines
      Strikeout inlines -> extractText inlines
      Superscript inlines -> extractText inlines
      Subscript inlines -> extractText inlines
      SmallCaps inlines -> extractText inlines
      Quoted SingleQuote inlines -> "'" <> extractText inlines <> "'"
      Quoted DoubleQuote inlines -> "\"" <> extractText inlines <> "\""
      Code _ s -> s
      Link _ inlines (url, _) ->
        let linkText = extractText inlines
         in if linkText == "" then url else linkText
      _ -> ""

-- | Render inline elements as HTML, preserving formatting
renderInlines :: [Inline] -> Html ()
renderInlines = mapM_ renderInline
  where
    renderInline = \case
      Str s -> toHtml s
      Space -> toHtml (" " :: Text)
      SoftBreak -> toHtml (" " :: Text)
      LineBreak -> toHtml ("\n" :: Text)
      Emph inlines -> em_ $ renderInlines inlines
      Strong inlines -> strong_ $ renderInlines inlines
      Underline inlines -> span_ [style_ "text-decoration: underline"] $ renderInlines inlines
      Strikeout inlines -> del_ $ renderInlines inlines
      Superscript inlines -> sup_ $ renderInlines inlines
      Subscript inlines -> sub_ $ renderInlines inlines
      SmallCaps inlines -> span_ [style_ "font-variant: small-caps"] $ renderInlines inlines
      Quoted SingleQuote inlines -> toHtml ("'" :: Text) *> renderInlines inlines *> toHtml ("'" :: Text)
      Quoted DoubleQuote inlines -> toHtml ("\"" :: Text) *> renderInlines inlines *> toHtml ("\"" :: Text)
      Code _ s -> code_ $ toHtml s
      Link _ inlines (url, title) ->
        if title /= ""
          then a_ [href_ url, title_ title] $ renderInlines inlines
          else a_ [href_ url] $ renderInlines inlines
      _ -> pass
