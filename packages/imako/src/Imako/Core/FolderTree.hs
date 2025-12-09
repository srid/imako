{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core.FolderTree (
  FolderNode (..),
  buildFolderTree,
  hasDueTasks,
  isTaskDue,
) where

import Data.Map.Strict qualified as Map
import Data.Time (Day)
import Ob (Task)
import Ob.Task (TaskProperties (..))
import Ob.Task qualified
import System.FilePath (splitDirectories)

-- | Hierarchical folder structure for organizing tasks
data FolderNode = FolderNode
  { subfolders :: Map Text FolderNode
  , files :: Map Text [Task]
  }
  deriving stock (Show, Eq)

-- | Build a folder tree from a map of file paths to task lists
buildFolderTree :: Map FilePath [Task] -> FolderNode
buildFolderTree = Map.foldlWithKey' insertFile emptyNode
  where
    emptyNode = FolderNode Map.empty Map.empty

    insertFile :: FolderNode -> FilePath -> [Task] -> FolderNode
    insertFile node filepath tasks =
      let parts = splitDirectories filepath
       in insertPath node parts tasks

    insertPath :: FolderNode -> [FilePath] -> [Task] -> FolderNode
    insertPath node [] _ = node -- shouldn't happen
    insertPath node [filename] tasks =
      -- Leaf: add file to current node
      node {files = Map.insert (toText filename) tasks node.files}
    insertPath node (folder : rest) tasks =
      -- Branch: recurse into subfolder
      let subfolderName = toText folder
          existingSubfolder = Map.findWithDefault emptyNode subfolderName node.subfolders
          updatedSubfolder = insertPath existingSubfolder rest tasks
       in node {subfolders = Map.insert subfolderName updatedSubfolder node.subfolders}

-- | Check if a task is due (incomplete/in-progress and due date <= today)
isTaskDue :: Day -> Task -> Bool
isTaskDue today task =
  task.status `elem` [Ob.Task.Incomplete, Ob.Task.InProgress]
    && maybe False (<= today) task.properties.dueDate

-- | Recursively check if folder node has any due tasks
hasDueTasks :: Day -> FolderNode -> Bool
hasDueTasks today node =
  let
    -- Check files in this folder
    filesDue = any (any (isTaskDue today)) (Map.elems node.files)
    -- Check subfolders
    subfoldersDue = any (hasDueTasks today) (Map.elems node.subfolders)
   in
    filesDue || subfoldersDue
