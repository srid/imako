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
import Data.Time (Day, UTCTime (..), addDays, getCurrentTime)
import Imako.CLI qualified as CLI
import Imako.UI.FolderTree (buildFolderTree, renderFolderTree)
import Imako.UI.Layout (layout)
import Imako.UI.PWA (imakoManifest)
import Imako.UI.Tasks (taskItem)
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
import Web.TablerIcons.Outline qualified as Icon

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

{- | Filter tasks completed in the last 7 days.

Returns completed tasks with a completion date within the last 7 days,
sorted by completion date (most recent first).
-}
filterRecentlyCompleted :: Day -> [Task] -> [Task]
filterRecentlyCompleted today tasks =
  let sevenDaysAgo = addDays (-7) today
      isRecentlyCompleted task =
        task.status == Completed
          && case task.properties.completedDate of
            Just date -> date >= sevenDaysAgo && date <= today
            Nothing -> False
   in sortOn (Down . (.properties.completedDate)) $
        filter isRecentlyCompleted tasks

-- | Group tasks by their source file path (relative to vault)
groupTasksByFile :: FilePath -> [Task] -> Map FilePath [Task]
groupTasksByFile vaultPath =
  List.foldl
    ( \acc task ->
        let relativePath = makeRelative vaultPath task.sourceNote
         in Map.insertWith (flip (++)) relativePath [task] acc
    )
    Map.empty

birdsEyeView :: Int -> Int -> Int -> Html ()
birdsEyeView pendingCount completedCount notesCount =
  div_ [class_ "grid grid-cols-3 gap-4 mb-8"] $ do
    card "Pending" "text-indigo-600 dark:text-indigo-400" pendingCount
    card "Completed" "text-green-600 dark:text-green-400" completedCount
    card "Notes" "text-gray-900 dark:text-gray-100" notesCount
  where
    card :: Text -> Text -> Int -> Html ()
    card label colorClass count =
      div_ [class_ "bg-white dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 p-5 shadow-sm"] $ do
        div_ [class_ "text-sm font-medium text-gray-500 dark:text-gray-400 mb-1"] $ toHtml label
        div_ [class_ ("text-3xl font-bold " <> colorClass)] $ toHtml (show count :: Text)

renderMainContent :: Day -> FilePath -> Ob.Vault -> Html ()
renderMainContent today vaultPath vault = do
  let (pendingCount, completedCount, filteredCount, groupedTasks) = processTasksForUI today vaultPath (getTasks vault)
      recentlyCompleted = filterRecentlyCompleted today (getTasks vault)
      groupedRecent = groupTasksByFile vaultPath recentlyCompleted

  -- Show filtered tasks message if any
  when (filteredCount > 0) $
    div_ [class_ "mb-4 p-4 bg-blue-50 dark:bg-blue-900/30 border border-blue-200 dark:border-blue-700 rounded-lg"] $
      p_ [class_ "text-sm text-blue-700 dark:text-blue-300"] $
        toHtml ("Hiding " <> show filteredCount <> " task" <> (if filteredCount == 1 then "" else "s" :: Text) <> " with start date 2+ days in future")

  -- Stats overview section
  birdsEyeView pendingCount completedCount (Map.size vault.notes)

  -- Tasks section with hierarchical folder structure
  div_ $ do
    let folderTree = buildFolderTree groupedTasks
    renderFolderTree vaultPath (`forM_` taskItem) folderTree

  -- Recently completed tasks section (last 7 days)
  unless (null recentlyCompleted) $
    div_ [class_ "mt-12 pt-8 border-t border-gray-200 dark:border-gray-700"] $ do
      details_ [class_ "", id_ "recently-completed-section", term "data-section" "recently-completed"] $ do
        summary_ [class_ "cursor-pointer text-lg font-semibold text-gray-700 dark:text-gray-300 mb-2 px-4 py-2 hover:bg-gray-100 dark:hover:bg-gray-800 rounded-lg flex items-center gap-2"] $ do
          div_ [class_ "w-3 h-3 flex-shrink-0 flex items-center justify-center transition-transform chevron-icon"] $
            toHtmlRaw Icon.chevron_right
          toHtml ("Recently Completed (" <> show (length recentlyCompleted) <> ")" :: Text)
        div_ [class_ "mt-4"] $ do
          p_
            [class_ "text-sm text-gray-600 dark:text-gray-400 mb-6 px-4"]
            "Tasks you've completed in the last 7 days. Keep up the momentum!"
          forM_ (Map.toList groupedRecent) $ \(filePath, tasks) ->
            div_ [class_ "mb-4"] $
              div_ [class_ "bg-gray-50 dark:bg-gray-800/50 rounded-lg border border-gray-200 dark:border-gray-700"] $ do
                h3_ [class_ "text-sm font-semibold text-gray-600 dark:text-gray-300 mb-3 px-4 pt-3"] $
                  strong_ $
                    toHtml filePath
                div_ [class_ "divide-y divide-gray-100 dark:divide-gray-700"] $
                  forM_ tasks taskItem

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:4009"
    Ob.withLiveVault options.path $ \vaultVar -> do
      S.scotty 4009 $ do
        S.get "/" $ do
          today <- liftIO $ utctDay <$> getCurrentTime
          vault <- liftIO $ LVar.get vaultVar
          S.html $
            renderText $
              layout
                ( do
                    "Imako: "
                    small_ [class_ "font-mono text-sm text-gray-500"] $ toHtml options.path
                )
                (renderMainContent today options.path vault)

        S.get "/manifest.json" $ do
          S.setHeader "Content-Type" "application/json"
          S.json imakoManifest

        S.get "/events" $ do
          S.setHeader "Content-Type" "text/event-stream"
          S.setHeader "Cache-Control" "no-cache"
          S.setHeader "Connection" "keep-alive"
          S.status status200
          S.stream $ \write flush -> forever $ do
            today <- utctDay <$> getCurrentTime
            vault <- LVar.listenNext vaultVar
            let html = renderText $ renderMainContent today options.path vault
            let sseData = "data: " <> html <> "\n\n"
            write $ lazyByteString $ TL.encodeUtf8 sseData
            flush
