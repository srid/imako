{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Imako.Core.FolderTree (
  FolderNode (..),
  buildFolderTree,
  annotateDailyNotes,
  hasDueTasks,
  isTaskDue,
  flattenTree,
  folderTreeTsDeclarations,
) where

import Data.Aeson (ToJSON, defaultOptions)
import Data.Aeson.TypeScript.Internal (TSDeclaration)
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.Map.Strict qualified as Map
import Data.Time (Day)
import Ob (DailyNote (..), Task)
import Ob.Task (TaskProperties (..))
import Ob.Task qualified
import System.FilePath (splitDirectories)

-- | Day serializes as ISO date string
instance TypeScript Day where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = []

data FolderNode = FolderNode
  { subfolders :: Map Text FolderNode
  , files :: Map Text [Task]
  , dailyNoteDates :: Maybe (Map Text Day)
  -- ^ Only set on the daily notes folder; maps filename to parsed date
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''FolderNode)

-- | All TypeScript declarations from this module
folderTreeTsDeclarations :: [TSDeclaration]
folderTreeTsDeclarations = getTypeScriptDeclarations (Proxy @FolderNode)

-- | Build a folder tree from a map of file paths to task lists
buildFolderTree :: Map FilePath [Task] -> FolderNode
buildFolderTree = Map.foldlWithKey' insertFile emptyNode
  where
    emptyNode = FolderNode Map.empty Map.empty Nothing

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
          existingSubfolder = Map.findWithDefault (FolderNode Map.empty Map.empty Nothing) subfolderName node.subfolders
          updatedSubfolder = insertPath existingSubfolder rest tasks
       in node {subfolders = Map.insert subfolderName updatedSubfolder node.subfolders}

{- | Annotate the daily notes folder in the tree with parsed dates.
Finds the subfolder matching the daily notes folder path and sets its dailyNoteDates.
-}
annotateDailyNotes :: [DailyNote] -> FilePath -> FolderNode -> FolderNode
annotateDailyNotes dailyNotes folderPath root =
  let dateMap =
        Map.fromList $
          map (\dn -> (toText $ takeFileName dn.notePath, dn.day)) dailyNotes
      folderName = toText folderPath
   in case Map.lookup folderName root.subfolders of
        Just sub ->
          let annotated = sub {dailyNoteDates = Just dateMap}
           in root {subfolders = Map.insert folderName annotated root.subfolders}
        Nothing -> root
  where
    takeFileName fp = case reverse (splitDirectories fp) of
      (f : _) -> f
      [] -> fp

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

-- | Recursively flatten folder ancestry where possible
flattenTree :: FolderNode -> FolderNode
flattenTree node =
  let flattenedSubs = Map.map flattenTree node.subfolders
      -- Check if a child folder can be merged with its parent
      -- (i.e. if the child has no files and exactly one subfolder)
      collapse :: (Text, FolderNode) -> (Text, FolderNode)
      collapse (name, n)
        | Map.null n.files && Map.size n.subfolders == 1 =
            case Map.toList n.subfolders of
              [(childName, childNode)] -> collapse (name <> "/" <> childName, childNode)
              _ -> (name, n) -- Should not happen
        | otherwise = (name, n)

      collapsedSubsList = map collapse (Map.toList flattenedSubs)
   in node {subfolders = Map.fromList collapsedSubsList}
