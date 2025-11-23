{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core.Filter where

import Data.Time (Day, addDays)
import Ob (Task (..))
import Ob.Task (TaskStatus (..))
import Ob.Task.Properties (TaskProperties (..))
import Text.Show (Show (show))

-- | A filter for task visibility
data Filter = Filter
  { filterId :: Text
  -- ^ Unique identifier for the filter (camelCase, used as storage key)
  , filterLabel :: Text
  -- ^ Human-readable label for the filter button
  , filterPredicate :: Day -> Task -> Bool
  -- ^ Predicate function to determine if a task matches this filter
  }

-- Manual instances since we can't derive for functions
instance Show Filter where
  show f = "Filter {filterId = \"" <> toString f.filterId <> "\", filterLabel = \"" <> toString f.filterLabel <> "\"}"

instance Eq Filter where
  -- Compare by filterId since predicates can't be compared
  (==) f1 f2 = f1.filterId == f2.filterId

-- | All available filters
filters :: [Filter]
filters =
  [ Filter
      { filterId = "showFuture"
      , filterLabel = "Future tasks"
      , filterPredicate = \today task ->
          let twoDaysFromNow = addDays 2 today
              isFuture = case task.properties.startDate of
                Just d -> d >= twoDaysFromNow
                Nothing -> False
              isParentFuture = any (\(_, start) -> maybe False (>= twoDaysFromNow) start) task.parentContext
           in isFuture || isParentFuture
      }
  , Filter
      { filterId = "showPast"
      , filterLabel = "Past tasks"
      , filterPredicate = \_ task -> task.status == Completed || task.status == Cancelled
      }
  ]
