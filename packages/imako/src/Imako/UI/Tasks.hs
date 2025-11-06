{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Tasks (
  taskItem,
  taskGroup,
  priorityText,
) where

import Data.Time (defaultTimeLocale, formatTime)
import Lucid
import Ob.Task (Priority (..), Task (..), TaskStatus (..), extractText, renderInlines)
import Ob.Task.Properties (TaskProperties (..))
import Ob.Task.Recurrence (formatRecurrence)
import System.FilePath (takeFileName)
import Web.TablerIcons.Outline qualified as Icon

-- | Task item component - displays a single task with checkbox and source
taskItem :: Task -> Html ()
taskItem task =
  let parentDescriptions = map fst task.parentContext
      indentLevel = length parentDescriptions
      indentClass = if indentLevel > 0 then "ml-" <> show (indentLevel * 4) else ""
      bgClass = case task.status of
        InProgress -> "bg-amber-50 dark:bg-amber-950/20"
        _ -> "bg-white dark:bg-gray-800"
      borderClass = case task.status of
        InProgress -> "border-l-2 border-amber-400 dark:border-amber-500"
        _ -> "border-l-2 border-transparent hover:border-indigo-400 dark:hover:border-indigo-500"
   in div_
        [class_ ("py-3 px-4 mb-1 hover:bg-gray-50 dark:hover:bg-gray-700 transition-colors " <> bgClass <> " " <> borderClass <> " " <> indentClass)]
        ( do
            div_ [class_ "flex items-start gap-4"] $ do
              -- Checkbox (larger, cleaner)
              div_ [class_ "w-5 h-5 flex-shrink-0 flex items-center justify-center"] $
                toHtmlRaw $
                  case task.status of
                    Completed -> Icon.square_check
                    Cancelled -> Icon.square_x
                    InProgress -> Icon.square_half
                    Incomplete -> Icon.square

              -- Main task text with breadcrumb
              div_ [class_ "flex-1 min-w-0"] $ do
                -- Show parent breadcrumb if exists
                unless (null parentDescriptions) $
                  div_ [class_ "text-xs text-gray-500 dark:text-gray-400 mb-1"] $
                    toHtml (formatBreadcrumb parentDescriptions)

                p_
                  [ title_ (extractText task.inlines)
                  , class_ $
                      "text-sm "
                        <> case task.status of
                          Completed -> "line-through text-gray-400 dark:text-gray-500"
                          Cancelled -> "line-through text-orange-400 dark:text-orange-500"
                          InProgress -> "text-amber-700 dark:text-amber-300 font-medium"
                          Incomplete -> "text-gray-900 dark:text-gray-100"
                  ]
                  $ renderInlines task.description

              -- Metadata pills (simplified, icon-only or minimal)
              div_ [class_ "flex items-center gap-1.5 flex-shrink-0"] $ do
                -- Priority indicator (icon only, no text)
                case task.properties.priority of
                  Normal -> mempty
                  Highest ->
                    div_ [title_ "Highest priority", class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-red-500 dark:text-red-400"] $ toHtmlRaw Icon.flame
                  High ->
                    div_ [title_ "High priority", class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-orange-500 dark:text-orange-400"] $ toHtmlRaw Icon.arrow_up
                  Medium ->
                    div_ [title_ "Medium priority", class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-yellow-500 dark:text-yellow-400"] $ toHtmlRaw Icon.chevron_up
                  Low ->
                    div_ [title_ "Low priority", class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-blue-500 dark:text-blue-400"] $ toHtmlRaw Icon.chevron_down
                  Lowest ->
                    div_ [title_ "Lowest priority", class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-gray-500 dark:text-gray-400"] $ toHtmlRaw Icon.arrow_down

                -- Dates (compact pill with icon + date)
                whenJust task.properties.dueDate $ \date ->
                  span_ [title_ "Due date", class_ "text-xs px-2 py-0.5 rounded bg-red-50 dark:bg-red-900/30 text-red-700 dark:text-red-300 border border-red-200 dark:border-red-700"] $
                    toHtml (formatTime defaultTimeLocale "%b %d" date)

                whenJust task.properties.scheduledDate $ \date ->
                  span_ [title_ "Scheduled", class_ "text-xs px-2 py-0.5 rounded bg-blue-50 dark:bg-blue-900/30 text-blue-700 dark:text-blue-300 border border-blue-200 dark:border-blue-700"] $
                    toHtml (formatTime defaultTimeLocale "%b %d" date)

                whenJust task.properties.startDate $ \date ->
                  span_ [title_ "Start date", class_ "text-xs px-2 py-0.5 rounded bg-purple-50 dark:bg-purple-900/30 text-purple-700 dark:text-purple-300 border border-purple-200 dark:border-purple-700"] $
                    toHtml (formatTime defaultTimeLocale "%b %d" date)

                -- Recurrence indicator
                whenJust task.properties.recurrence $ \recur ->
                  span_ [title_ (formatRecurrence recur), class_ "text-xs px-2 py-0.5 rounded bg-green-50 dark:bg-green-900/30 text-green-700 dark:text-green-300 border border-green-200 dark:border-green-700"] $
                    toHtml ("🔁 " <> formatRecurrence recur)

                -- Tags (subtle, icon only with count if multiple)
                unless (null task.properties.tags) $
                  span_ [title_ (show task.properties.tags), class_ "text-xs px-2 py-0.5 rounded bg-gray-100 dark:bg-gray-700 text-gray-600 dark:text-gray-300"] $
                    case task.properties.tags of
                      [tag] -> toHtml tag
                      tags -> toHtml (show (length tags) <> (" tags" :: Text))
        )

-- | Task group component - displays tasks for a source file
taskGroup :: FilePath -> [Task] -> Html ()
taskGroup sourceFile tasks = do
  div_ [class_ "mt-8 first:mt-0"] $ do
    -- File header with better spacing
    h3_ [class_ "text-sm font-semibold text-gray-600 dark:text-gray-300 mb-3 px-4"] $
      strong_ $
        toHtml (takeFileName sourceFile)
    -- Tasks with subtle background
    div_ [class_ "bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 divide-y divide-gray-100 dark:divide-gray-700"] $
      forM_ tasks taskItem

priorityText :: Priority -> Text
priorityText = \case
  Highest -> "⏫ Highest"
  High -> "🔺 High"
  Medium -> "🔼 Medium"
  Normal -> "Normal"
  Low -> "🔽 Low"
  Lowest -> "⏬ Lowest"

-- | Format parent task breadcrumb with truncation for long trails
formatBreadcrumb :: [Text] -> Text
formatBreadcrumb parents
  | length parents <= 3 = mconcat $ intersperse " > " parents
  | otherwise =
      let firstItems = take 2 parents
          lastItems = drop (length parents - 1) parents
       in mconcat $ intersperse " > " (firstItems <> ["..."] <> lastItems)
