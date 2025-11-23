{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.Filters (
  Filter (..),
  filters,
  renderFilterBar,
)
where

import Data.Time (Day, addDays)
import Lucid
import Ob (Task (..))
import Ob.Task (TaskStatus (..))
import Ob.Task.Properties (TaskProperties (..))

-- | A filter for task visibility
data Filter = Filter
  { filterId :: Text
  , filterLabel :: Text
  , filterClass :: Text
  , filterStorageKey :: Text
  , filterJsFunction :: Text
  , filterPredicate :: Day -> Task -> Bool
  }

-- | All available filters
filters :: [Filter]
filters =
  [ Filter
      { filterId = "future-tasks-toggle"
      , filterLabel = "Future tasks"
      , filterClass = "show-future"
      , filterStorageKey = "showFuture"
      , filterJsFunction = "toggleFilter('showFuture', 'show-future', 'task-content', 'future-tasks-toggle')"
      , filterPredicate = \today task ->
          let twoDaysFromNow = addDays 2 today
              isFuture = case task.properties.startDate of
                Just d -> d >= twoDaysFromNow
                Nothing -> False
              isParentFuture = any (\(_, start) -> maybe False (>= twoDaysFromNow) start) task.parentContext
           in isFuture || isParentFuture
      }
  , Filter
      { filterId = "past-tasks-toggle"
      , filterLabel = "Past tasks"
      , filterClass = "show-past"
      , filterStorageKey = "showPast"
      , filterJsFunction = "toggleFilter('showPast', 'show-past', 'task-content', 'past-tasks-toggle')"
      , filterPredicate = \_ task -> task.status == Completed || task.status == Cancelled
      }
  ]

-- | Render a single filter button
renderFilterButton :: Filter -> Html ()
renderFilterButton f =
  button_
    [ id_ f.filterId
    , class_ "px-3 py-1 text-xs font-medium rounded-full transition-colors bg-gray-100 dark:bg-gray-800 text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700 aria-pressed:bg-indigo-600 dark:aria-pressed:bg-indigo-500 aria-pressed:text-white dark:aria-pressed:text-white aria-pressed:hover:bg-indigo-700 dark:aria-pressed:hover:bg-indigo-400"
    , onclick_ f.filterJsFunction
    , term "aria-pressed" "false"
    ]
    (toHtml f.filterLabel)

-- | Render the filter bar with all filters
renderFilterBar :: Html ()
renderFilterBar =
  div_ [class_ "mb-4 flex items-center gap-2"] $
    forM_ filters renderFilterButton
