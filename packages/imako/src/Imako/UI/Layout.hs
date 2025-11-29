module Imako.UI.Layout (
  layout,
)
where

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
layout :: Text -> Html () -> Html ()
layout vaultPath mainContent =
  html_ $ do
    head_ $ do
      title_ "Imako"
      link_ [rel_ "icon", href_ "https://fav.farm/🌌"]
      pwaMeta
      -- Tailwind CSS with Typography plugin
      script_ [src_ "https://cdn.tailwindcss.com?plugins=typography"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.2/sse.js"] ("" :: Text)
      script_ [src_ "/app.js"] ("" :: Text)
    body_ [class_ "min-h-screen bg-gray-50 dark:bg-gray-900 font-sans text-gray-900 dark:text-gray-100"] $ do
      div_ [class_ "max-w-4xl mx-auto my-6"] $ do
        -- Vault path label attached to top of card
        div_ [class_ "text-center"] $
          span_ [class_ "inline-block px-3 py-1 text-xs font-mono bg-indigo-600 dark:bg-indigo-500 text-white rounded-t-lg"] $
            toHtml vaultPath
        -- Main content card
        div_ [class_ "bg-white dark:bg-gray-950 rounded-xl shadow-sm border border-indigo-600 dark:border-indigo-500 p-6 sm:p-8 -mt-px"] $ do
          -- Content (swapped by SSE)
          div_ [id_ "task-content", class_ "group", hxExt_ "sse", sseConnect_ "/events", sseSwap_ "message"] mainContent
