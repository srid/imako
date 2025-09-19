{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Components where

import Data.Time (defaultTimeLocale, formatTime)
import Lucid
import Ob.Task (Priority (..), Task (..), extractText)

-- | Site title bar component
titleBar :: Html () -> Html ()
titleBar content =
  header_ [class_ "bg-gray-900 text-white p-4 border-b"] $
    h1_ [class_ "text-xl font-semibold"] content

-- | Task item component - displays a single task with checkbox and source
taskItem :: Task -> Html ()
taskItem task =
  div_ [class_ "p-3 mb-2 bg-gray-50 rounded border"] $ do
    div_ [class_ "flex items-start gap-3"] $ do
      span_ [class_ "text-lg mt-1"] $ if task.isCompleted then "☑" else "☐"
      div_ [class_ "flex-1"] $ do
        -- Main description
        span_ [class_ (if task.isCompleted then "line-through text-gray-500" else "")] $
          toHtml (extractText task.description)

        -- Debug: Full inlines in gray
        div_ [class_ "mt-1 text-sm text-gray-400 font-mono"] $ do
          "Debug: " <> toHtml (extractText task.inlines)

        -- Properties section
        div_ [class_ "mt-2 flex flex-wrap gap-2 text-xs"] $ do
          -- Priority
          case task.priority of
            Normal -> mempty
            p ->
              span_ [class_ "px-2 py-1 bg-yellow-100 text-yellow-800 rounded"] $
                toHtml (priorityText p)

          -- Scheduled date
          case task.scheduledDate of
            Nothing -> mempty
            Just date ->
              span_ [class_ "px-2 py-1 bg-blue-100 text-blue-800 rounded"] $
                "⏳ " <> toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

          -- Due date
          case task.dueDate of
            Nothing -> mempty
            Just date ->
              span_ [class_ "px-2 py-1 bg-red-100 text-red-800 rounded"] $
                "📅 " <> toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

          -- Completed date
          case task.completedDate of
            Nothing -> mempty
            Just date ->
              span_ [class_ "px-2 py-1 bg-green-100 text-green-800 rounded"] $
                "✅ " <> toHtml (formatTime defaultTimeLocale "%Y-%m-%d" date)

          -- Tags
          forM_ task.tags $ \tag ->
            span_ [class_ "px-2 py-1 bg-gray-200 text-gray-700 rounded"] $
              toHtml tag

        -- Source note
        div_ [class_ "mt-2"] $
          small_ [class_ "text-gray-400 font-mono"] $
            "from " <> toHtml task.sourceNote

priorityText :: Priority -> Text
priorityText = \case
  Highest -> "⏫ Highest"
  High -> "🔺 High"
  Medium -> "🔼 Medium"
  Normal -> "Normal"
  Low -> "🔽 Low"
  Lowest -> "⏬ Lowest"
