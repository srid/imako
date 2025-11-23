{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Imako.Core.FolderTree (FolderNode, buildFolderTree)
import Ob (Task (..), TaskStatus (..), Vault)
import Ob.Vault (getTasks)
import System.FilePath (makeRelative)

{- | The view model for the application

Represents the processed state of the vault data, ready for UI rendering.
-}
newtype AppView = AppView
  { folderTree :: FolderNode
  -- ^ Tasks organized in a hierarchical folder tree structure
  }
  deriving stock (Show, Eq)

-- | Pure function to transform raw Vault data into AppView
mkAppView :: FilePath -> Vault -> AppView
mkAppView vaultPath vault =
  let tasks = getTasks vault
      incomplete = filter (\t -> t.status /= Completed && t.status /= Cancelled) tasks
      completedTasks = filter (\t -> t.status == Completed || t.status == Cancelled) tasks
      groupedAll =
        List.foldl
          ( \acc task ->
              let relativePath = makeRelative vaultPath task.sourceNote
               in Map.insertWith (flip (++)) relativePath [task] acc
          )
          Map.empty
          (incomplete <> completedTasks)
      tree = buildFolderTree groupedAll
   in AppView
        { folderTree = tree
        }
