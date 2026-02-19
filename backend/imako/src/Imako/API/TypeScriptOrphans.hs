{-# OPTIONS_GHC -Wno-orphans #-}

{- | Orphan TypeScript instances for external types (Day, UTCTime).
Kept in a single dedicated module so no other module needs -Wno-orphans.
-}
module Imako.API.TypeScriptOrphans () where

import Data.Aeson.TypeScript.TH (TypeScript (..))
import Data.Time (Day, UTCTime)

-- | Day serializes as ISO date string
instance TypeScript Day where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = []

-- | UTCTime serializes as ISO timestamp string
instance TypeScript UTCTime where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = []
