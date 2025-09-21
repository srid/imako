{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Components where

import Data.Time (defaultTimeLocale, formatTime)
import Lucid
import Ob.Task (Priority (..), Task (..), extractText)
import Ob.Task.Properties (TaskProperties (..))

-- | Site title bar component
titleBar :: Html () -> Html ()
titleBar content =
  header_ [class_ "bg-gray-900 text-white p-4 border-b"] $
    h1_ [class_ "text-xl font-semibold"] content

-- | Task item component - displays a single task with checkbox and source
taskItem :: Task -> Html ()
taskItem task =
  div_ [class_ "p-3 mb-2 bg-gray-50 rounded border"] $ do
    div_ [class_ "flex items-center gap-3"] $ do
      span_ [class_ "text-lg"] $ if task.isCompleted then "☑" else "☐"

      -- Main task text
      span_ [title_ (extractText task.inlines), class_ (if task.isCompleted then "line-through text-gray-500" else "")] $
        toHtml (extractText task.description)

      -- Metadata pills aligned to the right
      div_ [class_ "flex items-center gap-2 ml-auto"] $ do
        -- Priority pill
        case task.properties.priority of
          Normal -> mempty
          Highest ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-amber-800 text-amber-200"] $ do
              -- Flame icon (SVG)
              span_ [class_ "w-3 h-3"] "🔥"
              "Highest"
          High ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-orange-800 text-orange-200"] $ do
              span_ [class_ "w-3 h-3"] "🔺"
              "High"
          Medium ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-yellow-800 text-yellow-200"] $ do
              span_ [class_ "w-3 h-3"] "🔼"
              "Medium"
          Low ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-blue-800 text-blue-200"] $ do
              span_ [class_ "w-3 h-3"] "🔽"
              "Low"
          Lowest ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-gray-800 text-gray-200"] $ do
              span_ [class_ "w-3 h-3"] "⏬"
              "Lowest"

        -- Date pills (show all dates that exist)
        -- Start date
        case task.properties.startDate of
          Nothing -> mempty
          Just date ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-purple-900 text-purple-300"] $ do
              span_ [class_ "w-3 h-3"] "🛫"
              toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

        -- Scheduled date
        case task.properties.scheduledDate of
          Nothing -> mempty
          Just date ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-blue-900 text-blue-300"] $ do
              span_ [class_ "w-3 h-3"] "⏳"
              toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

        -- Due date
        case task.properties.dueDate of
          Nothing -> mempty
          Just date ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-red-900 text-red-300"] $ do
              span_ [class_ "w-3 h-3"] "📅"
              toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

        -- Completed date
        case task.properties.completedDate of
          Nothing -> mempty
          Just date ->
            span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-green-900 text-green-300"] $ do
              span_ [class_ "w-3 h-3"] "✅"
              toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

        -- Tags as generic pills
        forM_ task.properties.tags $ \tag ->
          span_ [class_ "inline-flex items-center gap-1.5 text-xs font-medium px-2 py-0.5 rounded-full bg-gray-700 text-gray-300"] $ do
            span_ [class_ "w-3 h-3"] "🏷️"
            toHtml tag

-- | Task group component - displays tasks for a source file
taskGroup :: FilePath -> [Task] -> Html ()
taskGroup sourceFile tasks = do
  h3_ [class_ "text-sm font-mono text-gray-400 mt-6 pb-2 border-b border-gray-700"] $
    toHtml sourceFile
  div_ [class_ "ml-4"] $
    forM_ tasks $ \task ->
      taskItem task

priorityText :: Priority -> Text
priorityText = \case
  Highest -> "⏫ Highest"
  High -> "🔺 High"
  Medium -> "🔼 Medium"
  Normal -> "Normal"
  Low -> "🔽 Low"
  Lowest -> "⏬ Lowest"
