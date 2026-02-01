{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- HLINT ignore "Use infinitely" -}

module Imako.Core (
  -- * App State
  AppState (..),
  withAppState,

  -- * Message building
  mkVaultInfo,
  mkTasksData,
  mkNotesData,
  mkServerMessage,
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, withAsync)
import Data.Aeson (object, toJSON, (.=))
import Data.LVar qualified as LVar
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.API.Protocol (NotesData (..), Query (..), QueryResponse (..), ServerMessage (..), TasksData (..), VaultInfo (..))
import Imako.Core.FolderTree (buildFolderTree)
import Imako.Core.FolderTree qualified as FolderTree
import Ob (Task (..), TaskStatus (..), Vault, noteToAst)
import Ob qualified
import Ob.Vault (getTasks, notes)
import System.FilePath (makeRelative, takeBaseName)

-- | Application state combining vault data with runtime context
data AppState = AppState
  { vault :: Vault
  , today :: Day
  }

-- | Get today's date
getLocalToday :: IO Day
getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

{- | Run with a live AppState that updates on:
- Vault file changes
- Day boundary crossings
-}
withAppState :: FilePath -> (LVar.LVar AppState -> IO ()) -> IO ()
withAppState path f = do
  Ob.withLiveVault path $ \vaultVar -> do
    initialVault <- LVar.get vaultVar
    initialToday <- getLocalToday
    appStateVar <- LVar.new (AppState initialVault initialToday)

    -- Run user action alongside watchers
    withAsync (f appStateVar) $ \_ -> do
      race_
        (watchVaultChanges vaultVar appStateVar)
        (watchDayChanges appStateVar)
  where
    -- Forward vault changes to AppState
    watchVaultChanges vaultVar appStateVar = forever $ do
      newVault <- LVar.listenNext vaultVar
      LVar.modify appStateVar (\s -> s {vault = newVault})

    -- Update AppState on day boundary
    watchDayChanges appStateVar = forever $ do
      startDay <- getLocalToday
      waitForDayChange startDay
      newDay <- getLocalToday
      LVar.modify appStateVar (\s -> AppState s.vault newDay)

    waitForDayChange startDay = fix $ \loop -> do
      threadDelay (60 * 1000000) -- Check every minute
      currentDay <- getLocalToday
      when (currentDay == startDay) loop

-- | Build vault info from path and app state
mkVaultInfo :: FilePath -> AppState -> VaultInfo
mkVaultInfo path appState =
  let todayVal = appState.today
   in VaultInfo
        { vaultPath = path
        , vaultName = toText $ takeBaseName path
        , today = todayVal
        }

-- | Build tasks data from app state
mkTasksData :: FilePath -> AppState -> TasksData
mkTasksData vaultPath appState =
  let tasks = getTasks appState.vault
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

-- | Build notes data by serializing the requested note to AST
mkNotesData :: FilePath -> Vault -> FilePath -> NotesData
mkNotesData _vaultPath vault reqPath =
  let ast = case Map.lookup reqPath vault.notes of
        Just note -> toJSON (Ob.noteToAst note)
        Nothing -> toJSON (object ["error" .= ("Note not found: " <> reqPath)])
   in NotesData {notePath = reqPath, noteAst = ast}

-- | Build server message for a query (pure - all state in AppState)
mkServerMessage :: FilePath -> AppState -> Query -> ServerMessage
mkServerMessage vaultPath appState query =
  ServerMessage
    { vaultInfo = mkVaultInfo vaultPath appState
    , response = case query of
        TasksQuery -> TasksResponse (mkTasksData vaultPath appState)
        NotesQuery path -> NotesResponse (mkNotesData vaultPath appState.vault path)
    }
