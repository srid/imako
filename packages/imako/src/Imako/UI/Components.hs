module Imako.UI.Components where

import Lucid

-- | Site title bar component
titleBar :: Html () -> Html ()
titleBar content =
  header_ [class_ "bg-gray-900 text-white p-4 border-b"] $
    h1_ [class_ "text-xl font-semibold"] content
