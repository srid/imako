{-# LANGUAGE OverloadedRecordDot #-}

{- | Daily notes UI rendering.

Displays a sidebar layout with dates on the left and note content on the right.
The vertical date sidebar acts like tabs, with today highlighted as the focal point.
-}
module Imako.UI.DailyNotes (
  renderThisMoment,
  renderDailyNoteWithSidebar,
) where

import Data.List qualified as List
import Data.Time (Day, defaultTimeLocale, formatTime)
import Imako.Core (AppView (..))
import Imako.UI.Tasks (obsidianEditButton)
import Imako.Web.Lucid (hxGet_, hxSwapOob_, hxSwap_, hxTarget_, liftHtml)
import Lucid
import Ob (DailyNote (..))
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

    -- Sidebar layout: dates on left, content on right (max height with scroll)
    div_ [class_ "flex max-h-96 bg-gradient-to-r from-indigo-50 to-purple-50 dark:from-indigo-950/30 dark:to-purple-950/30 rounded-lg border border-indigo-200 dark:border-indigo-800 overflow-hidden"] $ do
      -- Left sidebar: vertical date tabs
      liftHtml $ renderDateSidebar view.today view.today view.dailyNotes

      -- Right content: selected note (defaults to today)
      div_ [id_ "daily-note-content", class_ "flex-1 px-4 py-2 overflow-y-auto"] $
        renderNoteContentOnly view.today

-- | Render the vertical date sidebar with HTMX tab switching
renderDateSidebar :: Day -> Day -> [DailyNote] -> Html ()
renderDateSidebar today selectedDay notes = do
  nav_ [id_ "daily-note-sidebar", class_ "w-20 flex-shrink-0 bg-indigo-100/50 dark:bg-indigo-900/20 border-r border-indigo-200 dark:border-indigo-800 py-1"] $
    renderDateButtons today selectedDay notes

-- | Render the date tab buttons (shared between initial render and OOB updates)
renderDateButtons :: Day -> Day -> [DailyNote] -> Html ()
renderDateButtons today selectedDay notes = do
  forM_ notes $ \note -> do
    let isToday = note.day == today
        isSelected = note.day == selectedDay
        baseClasses = "block w-full px-2 py-1.5 text-center transition-colors cursor-pointer text-sm"
        selectedClasses = "bg-indigo-500 text-white font-medium"
        unselectedClasses = "text-gray-600 dark:text-gray-400 hover:bg-indigo-200/50 dark:hover:bg-indigo-800/30 hover:text-gray-900 dark:hover:text-gray-200"
    button_
      [ hxGet_ ("/daily/" <> formatDayISO note.day)
      , hxTarget_ "#daily-note-content"
      , hxSwap_ "innerHTML"
      , class_ $ baseClasses <> " " <> if isSelected then selectedClasses else unselectedClasses
      ]
      $ do
        -- Compact: "Sat 29" or "Sat 29 •" for today
        span_ [class_ "font-medium"] $ toHtml (formatDayName note.day)
        span_ $ toHtml (" " <> formatDayNumber note.day)
        when isToday $ span_ [class_ "ml-0.5"] "•"

-- | Render content for a specific day (internal, just the note content)
renderNoteContentOnly :: (MonadReader AppView m) => Day -> HtmlT m ()
renderNoteContentOnly selectedDay = do
  view <- ask
  let selectedNote = List.find (\dn -> dn.day == selectedDay) view.dailyNotes
  case selectedNote of
    Just note -> renderNoteView (note.day == view.today) note
    Nothing -> liftHtml $ renderNoNoteForDay selectedDay

{- | Render content + OOB sidebar update for HTMX endpoint
Returns both the note content AND an out-of-band sidebar swap
-}
renderDailyNoteWithSidebar :: (MonadReader AppView m) => Day -> HtmlT m ()
renderDailyNoteWithSidebar selectedDay = do
  view <- ask
  -- Main content (will replace #daily-note-content)
  renderNoteContentOnly selectedDay
  -- Out-of-band sidebar update (will replace #daily-note-sidebar)
  liftHtml $ renderDateSidebarOob view.today selectedDay view.dailyNotes

-- | Render sidebar with hx-swap-oob for out-of-band update
renderDateSidebarOob :: Day -> Day -> [DailyNote] -> Html ()
renderDateSidebarOob today selectedDay notes = do
  nav_ [id_ "daily-note-sidebar", hxSwapOob_ "true", class_ "w-20 flex-shrink-0 bg-indigo-100/50 dark:bg-indigo-900/20 border-r border-indigo-200 dark:border-indigo-800 py-1"] $
    renderDateButtons today selectedDay notes

-- | Render a daily note with its content
renderNoteView :: (MonadReader AppView m) => Bool -> DailyNote -> HtmlT m ()
renderNoteView _isToday note = do
  -- Container with relative positioning for edit button
  div_ [class_ "relative"] $ do
    -- Edit button (absolute top right, pulled into padding)
    div_ [class_ "absolute -top-6 -right-2"] $
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

-- | Render placeholder when no note exists for the selected day
renderNoNoteForDay :: Day -> Html ()
renderNoNoteForDay day = do
  div_ [class_ "flex items-center gap-3 text-gray-500 dark:text-gray-400"] $ do
    div_ [class_ "w-10 h-10 rounded-full bg-gray-200 dark:bg-gray-700 flex items-center justify-center"] $
      toHtmlRaw Icon.calendar_plus
    div_ $ do
      div_ [class_ "font-medium"] "No daily note"
      div_ [class_ "text-sm"] $
        toHtml ("No note for " <> formatDay day)

-- | Format a day as a full date (e.g., "Saturday, Nov 29")
formatDay :: Day -> Text
formatDay = toText . formatTime defaultTimeLocale "%A, %b %-d"

-- | Format day name (e.g., "Sat")
formatDayName :: Day -> Text
formatDayName = toText . formatTime defaultTimeLocale "%a"

-- | Format day number (e.g., "29")
formatDayNumber :: Day -> Text
formatDayNumber = toText . formatTime defaultTimeLocale "%-d"

-- | Format day as ISO date (e.g., "2025-11-29") for URL
formatDayISO :: Day -> Text
formatDayISO = toText . formatTime defaultTimeLocale "%Y-%m-%d"
