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
import Data.Time (Day, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Imako.CLI qualified as CLI
import Imako.UI.FolderTree (buildFolderTree, renderFolderTree)
import Imako.UI.Inbox (appendToInbox)
import Imako.UI.Layout (layout)
import Imako.UI.PWA (imakoManifest)
import Imako.UI.Tasks (fileTreeItem)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Ob qualified
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Vault (getTasks)
import Options.Applicative (execParser)
import System.FilePath (makeRelative, (</>))
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
processTasksForUI :: FilePath -> [Task] -> (Int, Int, Map FilePath [Task])
processTasksForUI vaultPath tasks =
  let incomplete = filter (\t -> t.status /= Completed && t.status /= Cancelled) tasks
      completedTasks = filter (\t -> t.status == Completed || t.status == Cancelled) tasks
      groupedAll =
        List.foldl
          ( \acc task ->
              let relativePath = makeRelative vaultPath task.sourceNote
               in Map.insertWith (flip (++)) relativePath [task] acc
          )
          Map.empty
          (incomplete <> completedTasks)
      -- Only show files that have at least one incomplete task
      grouped = Map.filter (any (\t -> t.status /= Completed && t.status /= Cancelled)) groupedAll
   in (length incomplete, length completedTasks, grouped)

renderMainContent :: Day -> FilePath -> Ob.Vault -> Html ()
renderMainContent today vaultPath vault = do
  let (_pendingCount, _completedCount, groupedTasks) = processTasksForUI vaultPath (getTasks vault)

  -- Filter Bar
  div_ [class_ "mb-4 flex items-center gap-2"] $ do
    -- Future Tasks Toggle
    button_
      [ id_ "future-tasks-toggle"
      , class_ "px-3 py-1 text-xs font-medium rounded-full transition-colors bg-gray-100 dark:bg-gray-800 text-gray-500 dark:text-gray-400 hover:bg-gray-200 dark:hover:bg-gray-700 aria-pressed:bg-indigo-600 dark:aria-pressed:bg-indigo-500 aria-pressed:text-white dark:aria-pressed:text-white aria-pressed:hover:bg-indigo-700 dark:aria-pressed:hover:bg-indigo-400"
      , onclick_ "toggleFutureTasks()"
      , term "aria-pressed" "false"
      ]
      "Future tasks"

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
    let protocol = case options.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"
        url = protocol <> "://" <> options.host <> ":" <> show options.port
    putTextLn $ "Starting web server on " <> url
    Ob.withLiveVault options.path $ \vaultVar -> do
      app <- S.scottyApp $ do
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

      let settings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setPort options.port
              & Warp.setTimeout 600 -- 10 minutes (for long SSE connections)
          tlsStateDir = options.path </> ".imako"
      liftIO $ startWarpServer settings tlsStateDir options.tlsConfig app
