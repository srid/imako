{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core (
  AppView (..),
  mkAppView,
)
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day)
import Imako.Core.Filter (Filter)
import Imako.Core.Filter qualified as FilterDef
import Imako.Core.FolderTree (FolderNode, buildFolderTree)
import Ob (Task (..), TaskStatus (..), Vault)
import Ob.Vault (getTasks)
import System.FilePath (makeRelative)

{- | The view model for the application

Represents the processed state of the vault data, ready for UI rendering.
-}
data AppView = AppView
  { folderTree :: FolderNode
  -- ^ Tasks organized in a hierarchical folder tree structure
  , filters :: [Filter]
  -- ^ Available filters for task visibility
  , today :: Day
  -- ^ Current date for date-based comparisons and filtering
  , vaultPath :: FilePath
  -- ^ Path to the vault root directory
  }
  deriving stock (Show, Eq)

-- | Pure function to transform raw Vault data into AppView
mkAppView :: Day -> FilePath -> Vault -> AppView
mkAppView today vaultPath vault =
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
        , filters = FilterDef.filters
        , today = today
        , vaultPath = vaultPath
        }
