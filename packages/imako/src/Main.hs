{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infinitely" #-}

module Main where

import Data.ByteString.Builder (lazyByteString)
import Data.LVar qualified as LVar
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day, addDays, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Imako.CLI qualified as CLI
import Imako.UI.FolderTree (buildFolderTree, renderFolderTree)
import Imako.UI.Inbox (appendToInbox)
import Imako.UI.Layout (layout)
import Imako.UI.PWA (imakoManifest)
import Imako.UI.Tasks (fileTreeItem)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Ob qualified
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Task.Properties (TaskProperties (..))
import Ob.Vault (getTasks)
import Options.Applicative (execParser)
import System.FilePath (makeRelative)
import Web.Scotty qualified as S

{- | Process tasks for UI display with filtering and grouping.

Filters out completed, cancelled, and far-future tasks, then groups remaining tasks by file.

* If a task or any of its parent tasks has a start date >= 2 days from today, the entire subtree is filtered out
* Tasks are grouped by their source file path (relative to vault)

Returns a tuple of:

* Number of incomplete tasks to display
* Number of completed tasks
* Number of filtered (far-future) tasks
* Map of tasks grouped by file path
-}
processTasksForUI :: Day -> FilePath -> [Task] -> (Int, Int, Int, Map FilePath [Task])
processTasksForUI today vaultPath tasks =
  let twoDaysFromNow = addDays 2 today
      -- Check if task or any of its parents are far future
      isNotFarFuture task =
        let checkDate = \case
              Nothing -> True
              Just startDate -> startDate < twoDaysFromNow
            -- Check task's own start date
            taskNotFuture = checkDate task.properties.startDate
            -- Check all parent start dates
            parentsNotFuture = all (checkDate . snd) task.parentContext
         in taskNotFuture && parentsNotFuture
      incomplete = filter (\t -> t.status /= Completed && t.status /= Cancelled) tasks
      incompleteNotFuture = filter isNotFarFuture incomplete
      filtered = length incomplete - length incompleteNotFuture
      completed = length tasks - length incomplete
      grouped =
        List.foldl
          ( \acc task ->
              let relativePath = makeRelative vaultPath task.sourceNote
               in Map.insertWith (flip (++)) relativePath [task] acc
          )
          Map.empty
          incompleteNotFuture
   in (length incompleteNotFuture, completed, filtered, grouped)

renderMainContent :: Day -> FilePath -> Ob.Vault -> Html ()
renderMainContent today vaultPath vault = do
  let (_pendingCount, _completedCount, filteredCount, groupedTasks) = processTasksForUI today vaultPath (getTasks vault)

  -- Show filtered tasks message if any (subtle)
  when (filteredCount > 0) $
    div_ [class_ "mb-3 text-xs text-gray-400 dark:text-gray-600"] $
      toHtml ("Hiding " <> show filteredCount <> " future task" <> (if filteredCount == 1 then "" else "s" :: Text))

  -- Tasks section with hierarchical folder structure
  div_ $ do
    let folderTree = buildFolderTree groupedTasks
    renderFolderTree vaultPath (fileTreeItem today) folderTree

-- | Get the current day in the local timezone
getLocalToday :: IO Day
getLocalToday = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  pure $ localDay (utcToLocalTime tz now)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:4009"
    Ob.withLiveVault options.path $ \vaultVar -> do
      S.scotty 4009 $ do
        S.get "/" $ do
          today <- liftIO getLocalToday
          vault <- liftIO $ LVar.get vaultVar
          S.html $
            renderText $
              layout (toText options.path) (renderMainContent today options.path vault)

        S.post "/inbox/add" $ do
          taskText <- S.formParam "text"
          liftIO $ appendToInbox options.path taskText
          S.text "OK"

        S.get "/manifest.json" $ do
          S.setHeader "Content-Type" "application/json"
          S.json imakoManifest

        S.get "/events" $ do
          S.setHeader "Content-Type" "text/event-stream"
          S.setHeader "Cache-Control" "no-cache"
          S.setHeader "Connection" "keep-alive"
          S.status status200
          S.stream $ \write flush -> forever $ do
            today <- getLocalToday
            vault <- LVar.listenNext vaultVar
            let html = renderText $ renderMainContent today options.path vault
            let sseData = "data: " <> html <> "\n\n"
            write $ lazyByteString $ TL.encodeUtf8 sseData
            flush
