module Imako.UI.Layout (
  layout,
) where

import Lucid

-- | Main page layout with HTML structure, head, and body
layout :: Html () -> Html () -> Html ()
layout titleContent mainContent =
  html_ $ do
    head_ $ do
      title_ "Imako"
      link_ [rel_ "icon", href_ "https://fav.farm/🌌"]
      script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
    body_ [class_ "min-h-screen bg-gray-100"] $ do
      titleBar titleContent
      div_ [class_ "max-w-5xl mx-auto p-6"] mainContent
  where
    titleBar :: Html () -> Html ()
    titleBar content =
      header_ [class_ "bg-white border-b border-gray-200 p-6 shadow-sm"] $
        h1_ [class_ "text-2xl font-bold text-gray-900"] content
