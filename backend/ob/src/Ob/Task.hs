{-# LANGUAGE TemplateHaskell #-}

-- | Module for working with obsidian-tasks
module Ob.Task (
  Task (..),
  TaskStatus (..),
  IxTask,
  TaskIxs,
  Priority (..),
  TaskProperties (..),
  extractTasks,
  extractText,
  noteTasks,
  renderInlines,
  taskTsDeclarations,
)
where

import Data.Aeson (ToJSON (..), defaultOptions, object, (.=))
import Data.Aeson.TypeScript.Internal (TSDeclaration (..), TSField (..))
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Data.Time.Calendar (Day)
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
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''TaskStatus)

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
  , taskNum :: Word
  -- ^ 1-based DFS position within the source note (for ordering and identity)
  , parentTaskNum :: Maybe Word
  -- ^ taskNum of the direct parent task, if nested (Nothing for top-level)
  , parentBreadcrumbs :: [Text]
  -- ^ Ancestor task descriptions from root to immediate parent (for display)
  , parentStartDate :: Maybe Day
  -- ^ Inherited start date from closest ancestor that has one (for filtering)
  }
  deriving stock (Show, Eq)

instance Ord Task where
  compare = compare `on` ((.sourceNote) &&& (.taskNum))

type TaskIxs = '[FilePath, TaskStatus]

type IxTask = IxSet TaskIxs Task

instance Indexable TaskIxs Task where
  indices =
    ixList
      (ixFun $ one . (.sourceNote))
      (ixFun $ one . (.status))

-- | Extract tasks from a note and build an IxTask.
noteTasks :: FilePath -> Pandoc -> IxTask
noteTasks sourcePath pandoc =
  Ix.fromList $ extractTasks sourcePath pandoc

-- | Custom ToJSON instance that preserves Pandoc inlines for frontend rendering
instance ToJSON Task where
  toJSON task =
    object
      [ "description" .= task.description
      , "sourceNote" .= task.sourceNote
      , "status" .= task.status
      , "dueDate" .= task.properties.dueDate
      , "scheduledDate" .= task.properties.scheduledDate
      , "startDate" .= task.properties.startDate
      , "completedDate" .= task.properties.completedDate
      , "priority" .= task.properties.priority
      , "tags" .= task.properties.tags
      , "parentBreadcrumbs" .= task.parentBreadcrumbs
      , "parentStartDate" .= task.parentStartDate
      ]

-- | TypeScript instance for Task (custom ToJSON, so manual definition)
instance TypeScript Task where
  getTypeScriptType _ = "Task"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "Task"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ TSField False "description" "Inline[]" Nothing
            , TSField False "sourceNote" "string" Nothing
            , TSField False "status" "TaskStatus" Nothing
            , TSField True "dueDate" "string" Nothing
            , TSField True "scheduledDate" "string" Nothing
            , TSField True "startDate" "string" Nothing
            , TSField True "completedDate" "string" Nothing
            , TSField False "priority" "Priority" Nothing
            , TSField False "tags" "string[]" Nothing
            , TSField False "parentBreadcrumbs" "string[]" Nothing
            , TSField True "parentStartDate" "string" Nothing
            ]
        , interfaceDoc = Nothing
        }
    ]

-- | All TypeScript declarations from this module
taskTsDeclarations :: [TSDeclaration]
taskTsDeclarations =
  mconcat
    [ getTypeScriptDeclarations (Proxy @TaskStatus)
    , getTypeScriptDeclarations (Proxy @Task)
    ]

{- | Extract tasks from a Pandoc document.

Assigns 1-based taskNum in DFS order. Tracks parent taskNum for nested tasks.
-}
extractTasks :: FilePath -> Pandoc -> [Task]
extractTasks sourcePath (Pandoc _ blocks) =
  let (tasks, _) = extractFromBlocks sourcePath Nothing [] Nothing 1 blocks
   in tasks

-- | Extract tasks from a list of blocks, returning tasks and next available taskNum
extractFromBlocks :: FilePath -> Maybe Word -> [Text] -> Maybe Day -> Word -> [Block] -> ([Task], Word)
extractFromBlocks path parentNum crumbs inheritedStart nextNum =
  foldl' step ([], nextNum)
  where
    step (acc, n) block =
      let (ts, n') = extractFromBlock path parentNum crumbs inheritedStart n block
       in (acc <> ts, n')

-- | Extract tasks from a block, tracking parent task context
extractFromBlock :: FilePath -> Maybe Word -> [Text] -> Maybe Day -> Word -> Block -> ([Task], Word)
extractFromBlock path parentNum crumbs inheritedStart nextNum = \case
  BulletList items -> foldItems items
  OrderedList _ items -> foldItems items
  _ -> ([], nextNum)
  where
    foldItems = foldl' step ([], nextNum)
    step (acc, n) item =
      let (ts, n') = extractFromItem path parentNum crumbs inheritedStart n item
       in (acc <> ts, n')

-- | Extract task from a list item, tracking parent tasks
extractFromItem :: FilePath -> Maybe Word -> [Text] -> Maybe Day -> Word -> [Block] -> ([Task], Word)
extractFromItem sourcePath parentNum crumbs inheritedStart nextNum blocks =
  let (maybeTask, nestedBlocks) = extractTaskFromBlocks blocks
      -- Assign taskNum and build task
      (thisTask, thisNum, thisCrumbs, childStart, nextAfterThis) = case maybeTask of
        Just mkTask ->
          let task = mkTask nextNum parentNum crumbs inheritedStart
              newCrumbs = crumbs <> [extractText task.description]
              -- Children inherit: task's own startDate (if any), otherwise the inherited one
              effectiveStart = task.properties.startDate <|> inheritedStart
           in ([task], Just nextNum, newCrumbs, effectiveStart, nextNum + 1)
        Nothing -> ([], parentNum, crumbs, inheritedStart, nextNum)
      -- Extract nested tasks with this task as parent (if it exists)
      (nestedTasks, nextFinal) = extractFromBlocks sourcePath thisNum thisCrumbs childStart nextAfterThis nestedBlocks
   in (thisTask <> nestedTasks, nextFinal)
  where
    -- Split blocks into task info and nested blocks
    extractTaskFromBlocks :: [Block] -> (Maybe (Word -> Maybe Word -> [Text] -> Maybe Day -> Task), [Block])
    extractTaskFromBlocks = \case
      Plain inlines : rest ->
        let mkTask = extractFromInlines sourcePath inlines
         in (mkTask, rest)
      Para inlines : rest ->
        let mkTask = extractFromInlines sourcePath inlines
         in (mkTask, rest)
      _ -> (Nothing, blocks)

-- | Extract task from inline elements, returning a task constructor
extractFromInlines :: FilePath -> [Inline] -> Maybe (Word -> Maybe Word -> [Text] -> Maybe Day -> Task)
extractFromInlines sourcePath = \case
  Str marker : Space : rest
    | Just taskStatus <- parseCheckbox marker ->
        Just $ \num pNum bc pStart -> parseTaskWithMetadata rest sourcePath taskStatus num pNum bc pStart
  _ -> Nothing
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
parseTaskWithMetadata :: [Inline] -> FilePath -> TaskStatus -> Word -> Maybe Word -> [Text] -> Maybe Day -> Task
parseTaskWithMetadata taskInlines sourcePath taskStatus num pNum breadcrumbs pStartDate =
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
        , taskNum = num
        , parentTaskNum = pNum
        , parentBreadcrumbs = breadcrumbs
        , parentStartDate = pStartDate
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
