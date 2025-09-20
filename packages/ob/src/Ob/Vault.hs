-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
)
where

import Data.Map.Strict qualified as Map
import Ob.Note (Note)
import Ob.Task (Task)

data Vault = Vault
  { notes :: Map.Map FilePath Note
  , tasks :: [Task]
  }
