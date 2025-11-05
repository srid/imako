{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Tasks (
  taskItem,
  taskGroup,
  priorityText,
) where

import Data.Time (defaultTimeLocale, formatTime)
import Lucid
import Ob.Task (Priority (..), Task (..), extractText)
import Ob.Task.Properties (TaskProperties (..))
import System.FilePath (takeFileName)

-- | Task item component - displays a single task with checkbox and source
taskItem :: Task -> Html ()
taskItem task =
  div_ [class_ "py-3 px-4 mb-1 bg-white hover:bg-gray-50 border-l-2 border-transparent hover:border-indigo-400 transition-colors"] $ do
    div_ [class_ "flex items-start gap-4"] $ do
      -- Checkbox (larger, cleaner)
      span_ [class_ "text-xl leading-none mt-0.5"] $ if task.isCompleted then "☑" else "☐"

      -- Main task text (larger, more prominent)
      div_ [class_ "flex-1 min-w-0"] $
        p_ [title_ (extractText task.inlines), class_ ("text-sm " <> if task.isCompleted then "line-through text-gray-400" else "text-gray-900")] $
          toHtml (extractText task.description)

      -- Metadata pills (simplified, icon-only or minimal)
      div_ [class_ "flex items-center gap-1.5 flex-shrink-0"] $ do
        -- Priority indicator (icon only, no text)
        case task.properties.priority of
          Normal -> mempty
          Highest ->
            span_ [title_ "Highest priority", class_ "text-sm"] "🔥"
          High ->
            span_ [title_ "High priority", class_ "text-sm"] "🔺"
          Medium ->
            span_ [title_ "Medium priority", class_ "text-sm"] "🔼"
          Low ->
            span_ [title_ "Low priority", class_ "text-sm"] "🔽"
          Lowest ->
            span_ [title_ "Lowest priority", class_ "text-sm"] "⏬"

        -- Dates (compact pill with icon + date)
        case task.properties.dueDate of
          Nothing -> mempty
          Just date ->
            span_ [title_ "Due date", class_ "text-xs px-2 py-0.5 rounded bg-red-50 text-red-700 border border-red-200"] $
              toHtml (formatTime defaultTimeLocale "%b %d" date)

        case task.properties.scheduledDate of
          Nothing -> mempty
          Just date ->
            span_ [title_ "Scheduled", class_ "text-xs px-2 py-0.5 rounded bg-blue-50 text-blue-700 border border-blue-200"] $
              toHtml (formatTime defaultTimeLocale "%b %d" date)

        case task.properties.startDate of
          Nothing -> mempty
          Just date ->
            span_ [title_ "Start date", class_ "text-xs px-2 py-0.5 rounded bg-purple-50 text-purple-700 border border-purple-200"] $
              toHtml (formatTime defaultTimeLocale "%b %d" date)

        -- Tags (subtle, icon only with count if multiple)
        unless (null task.properties.tags) $
          span_ [title_ (show task.properties.tags), class_ "text-xs px-2 py-0.5 rounded bg-gray-100 text-gray-600"] $
            case task.properties.tags of
              [tag] -> toHtml tag
              tags -> toHtml (show (length tags) <> (" tags" :: Text))

-- | Task group component - displays tasks for a source file
taskGroup :: FilePath -> [Task] -> Html ()
taskGroup sourceFile tasks = do
  div_ [class_ "mt-8 first:mt-0"] $ do
    -- File header with better spacing
    h3_ [class_ "text-xs font-semibold uppercase tracking-wider text-gray-500 mb-3 px-4"] $
      strong_ $
        toHtml (takeFileName sourceFile)
    -- Tasks with subtle background
    div_ [class_ "bg-gray-50 rounded-lg border border-gray-200 divide-y divide-gray-100"] $
      forM_ tasks taskItem

priorityText :: Priority -> Text
priorityText = \case
  Highest -> "⏫ Highest"
  High -> "🔺 High"
  Medium -> "🔼 Medium"
  Normal -> "Normal"
  Low -> "🔽 Low"
  Lowest -> "⏬ Lowest"
