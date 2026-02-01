{-# LANGUAGE DeriveAnyClass #-}

module Imako.Core.Filter where

import Data.Aeson (ToJSON)

-- | A filter for task visibility (filtering happens client-side)
data Filter = Filter
  { filterId :: Text
  -- ^ Unique identifier for the filter (camelCase, used as storage key)
  , filterLabel :: Text
  -- ^ Human-readable label for the filter button
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | All available filters
filters :: [Filter]
filters =
  [ Filter
      { filterId = "showFuture"
      , filterLabel = "Future tasks"
      }
  , Filter
      { filterId = "showPast"
      , filterLabel = "Past tasks"
      }
  ]
