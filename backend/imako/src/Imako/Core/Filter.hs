{-# LANGUAGE DeriveAnyClass #-}

module Imako.Core.Filter (
  Filter (..),
  filters,
)
where

import Data.Aeson (FromJSON, ToJSON)

-- | Available task filters (sum type)
data Filter
  = ShowFuture
  | ShowPast
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | All available filters
filters :: [Filter]
filters = [ShowFuture, ShowPast]
