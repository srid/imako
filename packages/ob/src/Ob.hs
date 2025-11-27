{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note (..),
  Task (..),
  TaskStatus (..),
  Vault (..),
  getVault,
  withLiveVault,
)
where

import Ob.Note (Note (..))
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Vault (Vault (..), getVault, withLiveVault)
