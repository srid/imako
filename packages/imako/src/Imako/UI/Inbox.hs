{-# LANGUAGE OverloadedStrings #-}

module Imako.UI.Inbox (
  inboxInputForm,
  appendToInbox,
)
where

import Lucid
import System.Directory (doesFileExist)
import System.FilePath ((</>))

-- Custom htmx attributes using term syntax
hxPost_ :: Text -> Attributes
hxPost_ = term "hx-post"

hxSwap_ :: Text -> Attributes
hxSwap_ = term "hx-swap"

-- | Render the inbox quick-add input form
inboxInputForm :: Html ()
inboxInputForm = do
  div_ [class_ "mb-6"] $ do
    form_
      [ id_ "inbox-form"
      , hxPost_ "/inbox/add"
      , hxSwap_ "none"
      , class_ "flex gap-2"
      ]
      $ do
        input_
          [ type_ "text"
          , name_ "text"
          , placeholder_ "Quick add to inbox... (press Enter)"
          , class_ "flex-1 px-4 py-2 border border-gray-300 dark:border-gray-700 rounded-lg bg-white dark:bg-gray-900 text-gray-900 dark:text-gray-100 placeholder-gray-400 dark:placeholder-gray-500 focus:outline-none focus:ring-2 focus:ring-indigo-500 dark:focus:ring-indigo-400 focus:border-transparent"
          , autofocus_
          ]
        -- Hidden submit button (form submits on Enter)
        button_ [type_ "submit", class_ "hidden"] mempty

    -- Script to reset form after HTMX request
    script_ "document.getElementById('inbox-form').addEventListener('htmx:afterRequest', function() { this.reset(); });"

-- | Append a task item to INBOX.md in the vault
appendToInbox :: FilePath -> Text -> IO ()
appendToInbox vaultPath taskText = do
  let inboxPath = vaultPath </> "INBOX.md"
  fileExists <- doesFileExist inboxPath

  -- If file doesn't exist, create it with a header
  unless fileExists $ do
    writeFile inboxPath "# Inbox\n\n"

  -- Append the task item
  appendFile inboxPath ("- [ ] " <> toString taskText <> "\n")
