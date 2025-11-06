module Imako.UI.Layout (
  layout,
) where

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
      -- PWA meta tags
      meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
      meta_ [name_ "theme-color", content_ "#4F46E5"]
      meta_ [name_ "apple-mobile-web-app-capable", content_ "yes"]
      meta_ [name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent"]
      meta_ [name_ "apple-mobile-web-app-title", content_ "Imako"]
      link_ [rel_ "apple-touch-icon", href_ "https://fav.farm/🌌"]
      link_ [rel_ "manifest", href_ "/manifest.json"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.4"] ("" :: Text)
      script_ [src_ "https://unpkg.com/htmx-ext-sse@2.2.2/sse.js"] ("" :: Text)
    body_ [class_ "min-h-screen bg-gray-100 dark:bg-gray-900"] $ do
      titleBar titleContent
      div_ [class_ "max-w-5xl mx-auto p-6", hxExt_ "sse", sseConnect_ "/events", sseSwap_ "message"] mainContent
  where
    titleBar :: Html () -> Html ()
    titleBar content =
      header_ [class_ "bg-white dark:bg-gray-800 border-b border-gray-200 dark:border-gray-700 p-6 shadow-sm"] $
        h1_ [class_ "text-xl font-bold text-gray-900 dark:text-gray-100"] content
