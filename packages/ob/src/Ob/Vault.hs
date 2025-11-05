-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getTasks,
)
where

import Data.Map.Strict qualified as Map
import Ob.Note (Note (..))
import Ob.Task (Task)

-- | An Obsidian vault folder
newtype Vault = Vault
  { notes :: Map.Map FilePath Note
  }

getTasks :: Vault -> [Task]
getTasks vault = concatMap tasks (Map.elems vault.notes)
