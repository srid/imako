{-# LANGUAGE OverloadedStrings #-}

module Imako.UI.DailyNoteInput (
  dailyNoteInputForm,
  appendToDailyNote,
  AppendResult (..),
)
where

import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Lucid
import Ob (getTodayNotePath, loadDailyNotesConfig)
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- Custom htmx attributes using term syntax
hxPost_ :: Text -> Attributes
hxPost_ = term "hx-post"

hxSwap_ :: Text -> Attributes
hxSwap_ = term "hx-swap"

-- | Render the daily note input form (goes inside daily notes section)
dailyNoteInputForm :: Html ()
dailyNoteInputForm = do
  form_
    [ id_ "daily-note-form"
    , hxPost_ "/thought/add"
    , hxSwap_ "none"
    , class_ "flex gap-2 mb-3"
    ]
    $ do
      input_
        [ id_ "daily-note-input"
        , type_ "text"
        , name_ "text"
        , placeholder_ "Add to daily note..."
        , class_ "flex-1 px-3 py-1.5 text-sm border border-indigo-300 dark:border-indigo-700 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 placeholder-gray-400 dark:placeholder-gray-500 focus:outline-none focus:ring-2 focus:ring-indigo-500 dark:focus:ring-indigo-400 focus:border-transparent"
        ]
      -- Hidden submit button (form submits on Enter)
      button_ [type_ "submit", class_ "hidden"] mempty

  -- Script for form reset
  script_ $
    unlines
      [ "const dailyNoteForm = document.getElementById('daily-note-form');"
      , "dailyNoteForm.addEventListener('htmx:afterRequest', function(e) {"
      , "  if (e.detail.successful) {"
      , "    this.reset();"
      , "  } else {"
      , "    alert('No daily note exists for today. Create one in Obsidian first.');"
      , "  }"
      , "});"
      ]

-- | Result of appending to daily note
data AppendResult = Success | NoDailyNoteExists | NoDailyNotesConfig
  deriving stock (Show, Eq)

-- | Prepend a line to today's daily note (as first list item)
appendToDailyNote :: FilePath -> Text -> IO AppendResult
appendToDailyNote vaultPath thoughtText = do
  mConfig <- loadDailyNotesConfig vaultPath
  case mConfig of
    Nothing -> pure NoDailyNotesConfig
    Just config -> do
      today <- getLocalToday
      let notePath = vaultPath </> getTodayNotePath config today
      fileExists <- doesFileExist notePath
      if fileExists
        then do
          existingContent <- decodeUtf8 <$> readFileBS notePath
          let newLine = "- " <> thoughtText
              newContent = newLine <> "\n" <> existingContent
          writeFileText notePath newContent
          pure Success
        else pure NoDailyNoteExists
  where
    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime
