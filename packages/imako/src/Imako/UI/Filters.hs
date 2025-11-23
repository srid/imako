{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Filters (
  renderFilterBar,
)
where

import Imako.Core (AppView (..))
import Imako.Core.Filter (Filter (..))
import Imako.UI.Tasks (AppHtml)
import Lucid

-- | Render a single filter button
renderFilterButton :: (Monad m) => Filter -> HtmlT m ()
renderFilterButton f =
  let buttonId = f.filterId <> "-toggle"
      cssClass = "show-" <> f.filterId
      onclickHandler = "toggleFilter('" <> f.filterId <> "', '" <> cssClass <> "', 'task-content', '" <> buttonId <> "')"
   in button_
        [ id_ buttonId
        , class_ "px-3 py-1 text-xs font-medium rounded-full transition-colors bg-gray-100 dark:bg-gray-800 text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700 aria-pressed:bg-indigo-600 dark:aria-pressed:bg-indigo-500 aria-pressed:text-white dark:aria-pressed:text-white aria-pressed:hover:bg-indigo-700 dark:aria-pressed:hover:bg-indigo-400"
        , onclick_ onclickHandler
        , term "aria-pressed" "false"
        ]
        (toHtml f.filterLabel)

-- | Render the filter bar with all filters
renderFilterBar :: AppHtml ()
renderFilterBar = do
  filters <- lift $ asks (\view -> view.filters)
  div_ [class_ "mb-4 flex items-center gap-2"] $
    forM_ filters renderFilterButton
