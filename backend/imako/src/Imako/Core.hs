{-# LANGUAGE OverloadedRecordDot #-}

module Imako.Core (
  mkVaultInfo,
  mkTasksData,
  mkNotesData,
  mkServerMessage,
)
where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.API.Protocol (NotesData (..), Query (..), ServerMessage (..), TasksData (..), VaultInfo (..))
import Imako.Core.FolderTree (buildFolderTree)
import Imako.Core.FolderTree qualified as FolderTree
import Ob (Task (..), TaskStatus (..), Vault)
import Ob.Vault (getTasks, notes)
import System.FilePath (makeRelative, takeBaseName)

-- | Get today's date
getLocalToday :: IO Day
getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

-- | Build vault info from path and current day
mkVaultInfo :: FilePath -> Day -> VaultInfo
mkVaultInfo path today =
  VaultInfo
    { vaultPath = path
    , vaultName = toText $ takeBaseName path
    , today = today
    }

-- | Build tasks data from vault
mkTasksData :: FilePath -> Vault -> TasksData
mkTasksData vaultPath vault =
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
   in TasksData {folderTree = tree}

-- | Build notes data from vault
mkNotesData :: Vault -> NotesData
mkNotesData vault = NotesData {noteCount = Map.size vault.notes}

-- | Build server message for a query (fetches today internally)
mkServerMessage :: FilePath -> Vault -> Query -> IO ServerMessage
mkServerMessage vaultPath vault query = do
  today <- getLocalToday
  let vaultInfo = mkVaultInfo vaultPath today
  pure $ case query of
    TasksQuery -> TasksResultMsg vaultInfo (mkTasksData vaultPath vault)
    NotesQuery -> NotesResultMsg vaultInfo (mkNotesData vault)
