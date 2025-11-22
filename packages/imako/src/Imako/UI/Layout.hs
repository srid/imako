module Imako.UI.Layout (
  layout,
)
where

import Imako.UI.FolderTree (folderStateScript)
import Imako.UI.PWA (pwaMeta)
import Lucid

-- Custom htmx attributes using term syntax
hxExt_ :: Text -> Attributes
hxExt_ = term "hx-ext"

sseConnect_ :: Text -> Attributes
sseConnect_ = term "sse-connect"

sseSwap_ :: Text -> Attributes
sseSwap_ = term "sse-swap"

-- | Main page layout with HTML structure, head, and body
layout :: Html () -> Html () -> Html ()
layout titleContent mainContent =
  html_ $ do
    head_ $ do
      title_ "Imako"
      link_ [rel_ "icon", href_ "https://fav.farm/🌌"]
      pwaMeta
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.2/sse.js"] ("" :: Text)
      folderStateScript
    body_ [class_ "min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100"] $ do
      titleBar titleContent
      div_ [class_ "max-w-4xl mx-auto my-6 bg-white dark:bg-gray-950 rounded-xl shadow-sm border border-gray-200 dark:border-gray-800 p-6 sm:p-8", hxExt_ "sse", sseConnect_ "/events", sseSwap_ "message"] mainContent
  where
    titleBar :: Html () -> Html ()
    titleBar content =
      header_ [class_ "bg-white dark:bg-gray-950 border-b border-gray-200 dark:border-gray-800 px-6 py-4"] $
        div_ [class_ "max-w-4xl mx-auto"] $
          h1_ [class_ "text-lg font-bold text-gray-900 dark:text-gray-100"] content
