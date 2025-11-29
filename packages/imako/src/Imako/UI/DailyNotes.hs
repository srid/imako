{-# LANGUAGE OverloadedRecordDot #-}

{- | Daily notes UI rendering.

Displays the "this moment" view with today's daily note as the focal point,
recent notes for context, and tasks due today.
-}
module Imako.UI.DailyNotes (
  renderThisMoment,
) where

import Data.Time (Day, defaultTimeLocale, formatTime)
import Imako.Core (AppView (..))
import Imako.UI.Tasks (obsidianEditButton, taskTreeItem)
import Lucid
import Ob (DailyNote (..))
import System.FilePath (takeBaseName)
import Text.Pandoc (def, runPure, writeHtml5String)
import Text.Pandoc.Definition (Pandoc)
import Web.TablerIcons.Outline qualified as Icon

-- | Render the "This Moment" section - today's note + tasks due today
renderThisMoment :: (MonadReader AppView m) => HtmlT m ()
renderThisMoment = do
  view <- ask
  div_ [class_ "mb-6"] $ do
    -- Section header
    div_ [class_ "flex items-center gap-2 mb-3"] $ do
      div_ [class_ "text-indigo-500 dark:text-indigo-400"] $
        toHtmlRaw Icon.sun
      h2_ [class_ "text-lg font-semibold text-gray-800 dark:text-gray-200"] "This Moment"
      span_ [class_ "text-sm text-gray-500 dark:text-gray-400"] $
        toHtml (formatDay view.today)

    -- Today's note card
    div_ [class_ "bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 rounded-lg border border-indigo-200 dark:border-indigo-800 p-4"] $ do
      case view.todayNote of
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

    -- Recent notes (subtle context)
    unless (null view.recentNotes) $ do
      div_ [class_ "mt-4"] $ do
        div_ [class_ "flex items-center gap-2 mb-2 text-sm text-gray-500 dark:text-gray-400"] $ do
          toHtmlRaw Icon.history
          span_ "Recent"
        div_ [class_ "flex flex-wrap gap-2"] $
          forM_ view.recentNotes renderRecentNote

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

-- | Render a recent note as a compact chip
renderRecentNote :: (MonadReader AppView m) => DailyNote -> HtmlT m ()
renderRecentNote note = do
  vaultPath <- asks (.vaultPath)
  let vaultName = toText $ takeBaseName vaultPath
      obsidianUrl = "obsidian://open?vault=" <> vaultName <> "&file=" <> toText note.notePath
  a_
    [ href_ obsidianUrl
    , class_ "inline-flex items-center gap-1.5 px-2.5 py-1 rounded-full bg-gray-100 dark:bg-gray-800 text-sm text-gray-600 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700 hover:text-gray-900 dark:hover:text-gray-200 transition-colors"
    ]
    $ do
      toHtmlRaw Icon.file_text
      span_ $ toHtml (formatDayShort note.day)

-- | Format a day as a full date (e.g., "Saturday, Nov 29")
formatDay :: Day -> Text
formatDay = toText . formatTime defaultTimeLocale "%A, %b %-d"

-- | Format a day as a short date (e.g., "Nov 28")
formatDayShort :: Day -> Text
formatDayShort = toText . formatTime defaultTimeLocale "%b %-d"
