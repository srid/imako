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
import Imako.CLI qualified as CLI
import Imako.UI.FolderTree (buildFolderTree, renderFolderTree)
import Imako.UI.Layout (layout)
import Imako.UI.Tasks (taskItem)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Ob qualified
import Ob.Task (Task (..))
import Ob.Vault (getTasks)
import Options.Applicative (execParser)
import System.FilePath (makeRelative)
import Web.Scotty qualified as S

processTasksForUI :: FilePath -> [Task] -> (Int, Int, Map FilePath [Task])
processTasksForUI vaultPath tasks =
  let incomplete = filter (not . (.isCompleted)) tasks
      completed = length tasks - length incomplete
      grouped =
        List.foldl
          ( \acc task ->
              let relativePath = makeRelative vaultPath task.sourceNote
               in Map.insertWith (flip (++)) relativePath [task] acc
          )
          Map.empty
          incomplete
   in (length incomplete, completed, grouped)

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

renderMainContent :: FilePath -> Ob.Vault -> Html ()
renderMainContent vaultPath vault = do
  let (pendingCount, completedCount, groupedTasks) = processTasksForUI vaultPath (getTasks vault)

  -- Stats overview section
  birdsEyeView pendingCount completedCount (Map.size vault.notes)

  -- Tasks section with hierarchical folder structure
  div_ $ do
    let folderTree = buildFolderTree groupedTasks
    renderFolderTree (`forM_` taskItem) folderTree

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:3000"
    Ob.withLiveVault options.path $ \vaultVar -> do
      S.scotty 3000 $ do
        S.get "/" $ do
          vault <- liftIO $ LVar.get vaultVar
          S.html $
            renderText $
              layout
                ( do
                    "Imako: "
                    small_ [class_ "font-mono text-sm text-gray-500"] $ toHtml options.path
                )
                (renderMainContent options.path vault)

        S.get "/events" $ do
          S.setHeader "Content-Type" "text/event-stream"
          S.setHeader "Cache-Control" "no-cache"
          S.setHeader "Connection" "keep-alive"
          S.status status200
          S.stream $ \write flush -> forever $ do
            vault <- LVar.listenNext vaultVar
            let html = renderText $ renderMainContent options.path vault
            let sseData = "data: " <> html <> "\n\n"
            write $ lazyByteString $ TL.encodeUtf8 sseData
            flush
