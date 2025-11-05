module Imako.UI.Layout (
  titleBar,
) where

import Lucid

-- | Site title bar component
titleBar :: Html () -> Html ()
titleBar content =
  header_ [class_ "bg-white border-b border-gray-200 p-6 shadow-sm"] $
    h1_ [class_ "text-2xl font-bold text-gray-900"] content
