{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core (
  mkVaultInfo,
  mkTasksData,
  mkNotesData,
)
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day)
import Imako.API.Protocol (NotesData (..), TasksData (..), VaultInfo (..))
import Imako.Core.Filter qualified as FilterDef
import Imako.Core.FolderTree (buildFolderTree)
import Imako.Core.FolderTree qualified as FolderTree
import Ob (Task (..), TaskStatus (..), Vault)
import Ob.Vault (getTasks, notes)
import System.FilePath (makeRelative, takeBaseName)

-- | Build vault info from path
mkVaultInfo :: FilePath -> VaultInfo
mkVaultInfo path =
  VaultInfo
    { vaultPath = path
    , vaultName = toText $ takeBaseName path
    }

-- | Build tasks data from vault
mkTasksData :: Day -> FilePath -> Vault -> TasksData
mkTasksData today vaultPath vault =
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
      tree = FolderTree.flattenTree $ buildFolderTree groupedAll
   in TasksData
        { folderTree = tree
        , filters = FilterDef.filters
        , today = today
        }

-- | Build notes data from vault
mkNotesData :: Vault -> NotesData
mkNotesData vault = NotesData {noteCount = Map.size vault.notes}
