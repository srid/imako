{-# LANGUAGE OverloadedRecordDot #-}

{- | Daily notes UI rendering.

Displays a sidebar layout with dates on the left and note content on the right.
The vertical date sidebar acts like tabs, with today highlighted as the focal point.
-}
module Imako.UI.DailyNotes (
  renderThisMoment,
) where

import Data.List qualified as List
import Data.Time (Day, defaultTimeLocale, formatTime)
import Imako.Core (AppView (..))
import Imako.UI.Tasks (obsidianEditButton, taskTreeItem)
import Lucid
import Ob (DailyNote (..))
import System.FilePath (takeBaseName)
import Text.Pandoc (def, runPure, writeHtml5String)
import Text.Pandoc.Definition (Pandoc)
import Web.TablerIcons.Outline qualified as Icon

-- | Render the "This Moment" section with sidebar layout
renderThisMoment :: (MonadReader AppView m) => HtmlT m ()
renderThisMoment = do
  view <- ask
  div_ [class_ "mb-6"] $ do
    -- Section header
    div_ [class_ "flex items-center gap-2 mb-3"] $ do
      div_ [class_ "text-indigo-500 dark:text-indigo-400"] $
        toHtmlRaw Icon.sun
      h2_ [class_ "text-lg font-semibold text-gray-800 dark:text-gray-200"] "This Moment"

    -- Sidebar layout: dates on left, content on right
    div_ [class_ "flex gap-4 bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 rounded-lg border border-indigo-200 dark:border-indigo-800 overflow-hidden"] $ do
      -- Left sidebar: vertical date tabs
      renderDateSidebar view.today view.dailyNotes

      -- Right content: today's note + tasks
      div_ [class_ "flex-1 p-4"] $ do
        renderTodayContent view

-- | Render the vertical date sidebar
renderDateSidebar :: (MonadReader AppView m) => Day -> [DailyNote] -> HtmlT m ()
renderDateSidebar today notes = do
  vaultPath <- asks (.vaultPath)
  let vaultName = toText $ takeBaseName vaultPath
  nav_ [class_ "w-24 flex-shrink-0 bg-indigo-100/50 dark:bg-indigo-900/20 border-r border-indigo-200 dark:border-indigo-800 py-2"] $ do
    forM_ notes $ \note -> do
      let isToday = note.day == today
          obsidianUrl = "obsidian://open?vault=" <> vaultName <> "&file=" <> toText note.notePath
          baseClasses = "block w-full px-3 py-2 text-center transition-colors"
          todayClasses = "bg-indigo-500 text-white font-medium"
          otherClasses = "text-gray-600 dark:text-gray-400 hover:bg-indigo-200/50 dark:hover:bg-indigo-800/30 hover:text-gray-900 dark:hover:text-gray-200"
      a_
        [ href_ obsidianUrl
        , class_ $ baseClasses <> " " <> if isToday then todayClasses else otherClasses
        ]
        $ do
          div_ [class_ "text-xs font-medium"] $
            toHtml (formatDayName note.day)
          div_ [class_ $ "text-lg " <> if isToday then "font-bold" else ""] $
            toHtml (formatDayNumber note.day)
          when isToday $
            div_ [class_ "text-xs opacity-80"] "Today"

-- | Render today's content (note + tasks)
renderTodayContent :: (MonadReader AppView m) => AppView -> HtmlT m ()
renderTodayContent view = do
  let todayNote = List.find (\dn -> dn.day == view.today) view.dailyNotes
  case todayNote of
    Just note -> renderTodayNote note
    Nothing -> renderNoTodayNote view.today

  -- Tasks due today
  unless (null view.todayTasks) $ do
    div_ [class_ "mt-4 pt-4 border-t border-indigo-200 dark:border-indigo-800"] $ do
      div_ [class_ "flex items-center gap-2 mb-2 text-sm font-medium text-gray-700 dark:text-gray-300"] $ do
        toHtmlRaw Icon.checkbox
        span_ $ toHtml ("Due today (" <> show (length view.todayTasks) <> ")" :: Text)
      div_ [class_ "flex flex-col gap-1"] $
        forM_ view.todayTasks taskTreeItem

-- | Render today's daily note with its content
renderTodayNote :: (MonadReader AppView m) => DailyNote -> HtmlT m ()
renderTodayNote note = do
  -- Note header with edit button
  div_ [class_ "group/today flex items-center justify-between mb-3"] $ do
    div_ [class_ "flex items-center gap-3"] $ do
      div_ [class_ "w-10 h-10 rounded-full bg-indigo-500 dark:bg-indigo-600 flex items-center justify-center text-white"] $
        toHtmlRaw Icon.calendar_event
      div_ $ do
        div_ [class_ "font-medium text-gray-900 dark:text-gray-100"] $
          toHtml (toText $ takeBaseName note.notePath)
        div_ [class_ "text-sm text-gray-600 dark:text-gray-400"] "Today's note"
    obsidianEditButton note.notePath

  -- Note content rendered from Pandoc
  div_ [class_ "prose prose-sm dark:prose-invert max-w-none"] $
    toHtmlRaw (renderPandoc note.noteContent)

-- | Render Pandoc document to HTML
renderPandoc :: Pandoc -> Text
renderPandoc doc =
  case runPure (writeHtml5String def doc) of
    Left _ -> ""
    Right html -> html

-- | Render placeholder when no today's note exists
renderNoTodayNote :: (Monad m) => Day -> HtmlT m ()
renderNoTodayNote today = do
  div_ [class_ "flex items-center gap-3 text-gray-500 dark:text-gray-400"] $ do
    div_ [class_ "w-10 h-10 rounded-full bg-gray-200 dark:bg-gray-700 flex items-center justify-center"] $
      toHtmlRaw Icon.calendar_plus
    div_ $ do
      div_ [class_ "font-medium"] "No daily note yet"
      div_ [class_ "text-sm"] $
        toHtml ("Create one for " <> formatDay today)

-- | Format a day as a full date (e.g., "Saturday, Nov 29")
formatDay :: Day -> Text
formatDay = toText . formatTime defaultTimeLocale "%A, %b %-d"

-- | Format day name (e.g., "Sat")
formatDayName :: Day -> Text
formatDayName = toText . formatTime defaultTimeLocale "%a"

-- | Format day number (e.g., "29")
formatDayNumber :: Day -> Text
formatDayNumber = toText . formatTime defaultTimeLocale "%-d"
