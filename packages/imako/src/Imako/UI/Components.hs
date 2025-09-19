module Imako.UI.Components where

import Lucid

-- | Site title bar component
titleBar :: Html () -> Html ()
titleBar content =
  header_ [class_ "bg-gray-900 text-white p-4 border-b"] $
    h1_ [class_ "text-xl font-semibold"] content

-- | Task item component - displays a single task with checkbox and source
taskItem :: Text -> FilePath -> Bool -> Html ()
taskItem taskText sourceNote isCompleted =
  div_ [class_ "p-2 mb-1 bg-gray-50 rounded flex items-center gap-2"] $ do
    span_ [class_ "text-lg"] $ if isCompleted then "☑" else "☐"
    div_ [class_ "flex-1"] $ do
      span_ [class_ (if isCompleted then "line-through text-gray-500" else "")] $
        toHtml taskText
      br_ []
      small_ [class_ "text-gray-400 font-mono"] $ "from " <> toHtml sourceNote
