{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Tasks (
  taskTreeItem,
  fileTreeItem,
)
where

import Data.Time (Day, defaultTimeLocale, formatTime)
import Lucid
import Ob.Task (Priority (..), Task (..), TaskStatus (..), renderInlines)
import Ob.Task.Properties (TaskProperties (..))
import Ob.Task.Recurrence (formatRecurrence)
import System.FilePath (takeFileName)
import Web.TablerIcons.Outline qualified as Icon

-- | Render a file as a tree node containing tasks
fileTreeItem :: Day -> FilePath -> [Task] -> Html ()
fileTreeItem today sourceFile tasks = do
  let filename = takeFileName sourceFile
      -- Calculate completion stats
      total = length tasks
      completed = length $ filter (\t -> t.status == Completed || t.status == Cancelled) tasks
      progress = if total == 0 then 0 else (fromIntegral completed / fromIntegral total) * 100 :: Double

  details_ [class_ "group/file mt-4 first:mt-0", open_ "", term "data-folder-path" (toText sourceFile)] $ do
    summary_ [class_ "list-none cursor-pointer -mx-2 px-3 py-1.5 rounded-md hover:bg-gray-50 dark:hover:bg-gray-800/50 flex items-center gap-2 text-sm font-medium text-gray-500 dark:text-gray-400 select-none transition-colors mb-1"] $ do
      -- Chevron
      div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-400 transition-transform group-open/file:rotate-90"] $
        toHtmlRaw Icon.chevron_right

      -- Icon & Name
      div_ [class_ "flex items-center gap-2 flex-1 min-w-0"] $ do
        div_ [class_ "text-gray-300 dark:text-gray-600"] $ toHtmlRaw Icon.file
        span_ [class_ "truncate"] $ toHtml filename

        -- Progress bar (mini)
        when (total > 0) $
          div_ [class_ "ml-2 w-16 h-1 bg-gray-100 dark:bg-gray-800 rounded-full overflow-hidden"] $
            div_ [class_ "h-full bg-gray-300 dark:bg-gray-600", style_ ("width: " <> show progress <> "%")] mempty

      -- Count
      span_ [class_ "text-xs text-gray-300 dark:text-gray-600 font-normal mr-2"] $
        toHtml ((show completed <> "/" <> show total) :: Text)

      -- Edit link
      let obsidianUrl = "obsidian://open?path=" <> toText sourceFile
      a_ [href_ obsidianUrl, class_ "opacity-0 group-hover/file:opacity-100 p-1 -mr-1 rounded hover:bg-gray-100 dark:hover:bg-gray-800 text-gray-400 hover:text-indigo-600 transition-all", title_ "Edit in Obsidian", onclick_ "event.stopPropagation()"] $
        div_ [class_ "w-4 h-4"] $
          toHtmlRaw Icon.edit

    -- Tasks list (indented)
    div_ [class_ "pl-8 flex flex-col"] $
      forM_ tasks (taskTreeItem today)

-- | Render a single task as a tree item row
taskTreeItem :: Day -> Task -> Html ()
taskTreeItem today task = do
  let parentDescriptions = map fst task.parentContext
      indentLevel = length parentDescriptions
      -- Visual indentation for hierarchy (increased to 2rem per level)
      indentStyle = if indentLevel > 0 then "padding-left: " <> show (indentLevel * 2) <> "rem" else ""

      (statusColor, textStyle) = case task.status of
        Completed -> ("text-gray-400", "line-through text-gray-400 dark:text-gray-500")
        Cancelled -> ("text-gray-400", "line-through text-gray-400 dark:text-gray-500")
        InProgress -> ("text-amber-500", "text-gray-900 dark:text-gray-100")
        Incomplete -> ("text-gray-400 hover:text-gray-600", "text-gray-900 dark:text-gray-100")

  div_ [class_ "group/task relative py-1 -mx-2 px-2 rounded hover:bg-gray-50 dark:hover:bg-gray-800/50 flex items-start gap-2 text-sm transition-colors", style_ indentStyle] $ do
    -- Thread line for indented items
    when (indentLevel > 0) $
      div_ [class_ "absolute top-0 bottom-0 w-px bg-gray-200 dark:bg-gray-800", style_ ("left: " <> show (indentLevel * 2 - 1) <> "rem")] mempty

    -- Checkbox
    button_ [class_ ("w-5 h-5 flex-shrink-0 flex items-center justify-center transition-colors " <> statusColor)] $
      toHtmlRaw $
        case task.status of
          Completed -> Icon.square_check
          Cancelled -> Icon.square_x
          InProgress -> Icon.square_half
          Incomplete -> Icon.square

    -- Content
    div_ [class_ "flex-1 min-w-0"] $ do
      div_ [class_ ("leading-snug " <> textStyle)] $
        renderInlines task.description

      -- Metadata (Inline, subtle)
      let hasMetadata = not (null task.properties.tags) || isJust task.properties.dueDate || isJust task.properties.scheduledDate || isJust task.properties.startDate || isJust task.properties.completedDate || isJust task.properties.recurrence || task.properties.priority /= Normal
      when hasMetadata $
        div_ [class_ "flex flex-wrap items-center gap-x-3 gap-y-1 mt-0.5 text-xs text-gray-400 dark:text-gray-500"] $ do
          -- Priority
          case task.properties.priority of
            Normal -> mempty
            p -> do
              let pColor = case p of
                    Highest -> "text-red-600 dark:text-red-400"
                    High -> "text-orange-600 dark:text-orange-400"
                    Medium -> "text-amber-600 dark:text-amber-400"
                    Low -> "text-blue-600 dark:text-blue-400"
                    Lowest -> "text-slate-500 dark:text-slate-400"
              span_ [class_ ("flex items-center gap-0.5 font-medium " <> pColor)] $ do
                case p of
                  Highest -> toHtmlRaw Icon.flame
                  High -> toHtmlRaw Icon.arrow_up
                  Medium -> toHtmlRaw Icon.chevron_up
                  Low -> toHtmlRaw Icon.chevron_down
                  Lowest -> toHtmlRaw Icon.arrow_down
                toHtml (show p :: Text)

          -- Due Date
          whenJust task.properties.dueDate $ \d -> do
            let (dateColor, dateIcon) = case compare d today of
                  LT -> ("text-red-600 dark:text-red-400 font-medium", Icon.calendar_exclamation) -- Overdue
                  EQ -> ("text-amber-600 dark:text-amber-400 font-medium", Icon.calendar) -- Today
                  GT -> ("text-gray-500 dark:text-gray-400", Icon.calendar) -- Future
            span_ [class_ ("flex items-center gap-0.5 " <> dateColor), title_ "Due date"] $ do
              toHtmlRaw dateIcon
              toHtml (formatTime defaultTimeLocale "%b %d" d)

          -- Scheduled Date
          whenJust task.properties.scheduledDate $ \d ->
            span_ [class_ "flex items-center gap-0.5 text-blue-600 dark:text-blue-400", title_ "Scheduled"] $ do
              toHtmlRaw Icon.calendar_time
              toHtml (formatTime defaultTimeLocale "%b %d" d)

          -- Start Date
          whenJust task.properties.startDate $ \d ->
            span_ [class_ "flex items-center gap-0.5 text-purple-600 dark:text-purple-400", title_ "Start date"] $ do
              toHtmlRaw Icon.player_play
              toHtml (formatTime defaultTimeLocale "%b %d" d)

          -- Completed Date
          whenJust task.properties.completedDate $ \d ->
            span_ [class_ "flex items-center gap-0.5 text-teal-600 dark:text-teal-400", title_ "Completed"] $ do
              toHtmlRaw Icon.circle_check
              toHtml (formatTime defaultTimeLocale "%b %d" d)

          -- Recurrence
          whenJust task.properties.recurrence $ \recur ->
            span_ [class_ "flex items-center gap-0.5 text-green-600 dark:text-green-400", title_ (formatRecurrence recur)] $ do
              toHtmlRaw Icon.repeat
              toHtml ("🔁 " <> formatRecurrence recur)

          -- Tags
          unless (null task.properties.tags) $
            span_ [class_ "flex items-center gap-0.5 text-indigo-500 dark:text-indigo-400"] $ do
              toHtmlRaw Icon.tag
              toHtml (mconcat $ intersperse ", " task.properties.tags)
