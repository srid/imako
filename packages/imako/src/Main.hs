{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Imako.CLI qualified as CLI
import Imako.UI.Components (taskGroup, titleBar)
import Lucid
import Main.Utf8 qualified as Utf8
import Ob qualified
import Ob.Task (Task (..))
import Ob.Vault (getTasks)
import Options.Applicative (execParser)
import Web.Scotty qualified as S

processTasksForUI :: [Task] -> (Int, Int, Map FilePath [Task])
processTasksForUI tasks =
  let incomplete = filter (not . (.isCompleted)) tasks
      completed = length tasks - length incomplete
      grouped = List.foldl (\acc task -> Map.insertWith (flip (++)) task.sourceNote [task] acc) Map.empty incomplete
   in (length incomplete, completed, grouped)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:3000"
    Ob.withLiveVault options.path $ \vaultVar -> do
      S.scotty 3000 $ do
        S.get "/" $ do
          vault <- liftIO $ readTVarIO vaultVar
          S.html $ renderText $ html_ $ do
            head_ $ do
              title_ "Imako"
              link_ [rel_ "icon", href_ "https://fav.farm/🌌"]
              script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
            body_ [class_ "min-h-screen bg-gray-100"] $ do
              titleBar $ do
                "Imako: "
                small_ [class_ "font-mono text-sm text-gray-500"] $ toHtml options.path

              -- Main content area
              div_ [class_ "max-w-5xl mx-auto p-6"] $ do
                let (pendingCount, completedCount, groupedTasks) = processTasksForUI (getTasks vault)

                -- Stats overview section (bird's eye view)
                div_ [class_ "grid grid-cols-3 gap-4 mb-8"] $ do
                  -- Pending tasks card
                  div_ [class_ "bg-white rounded-lg border border-gray-200 p-5 shadow-sm"] $ do
                    div_ [class_ "text-sm font-medium text-gray-500 mb-1"] "Pending"
                    div_ [class_ "text-3xl font-bold text-indigo-600"] $ toHtml (show pendingCount :: Text)

                  -- Completed tasks card
                  div_ [class_ "bg-white rounded-lg border border-gray-200 p-5 shadow-sm"] $ do
                    div_ [class_ "text-sm font-medium text-gray-500 mb-1"] "Completed"
                    div_ [class_ "text-3xl font-bold text-green-600"] $ toHtml (show completedCount :: Text)

                  -- Notes card
                  div_ [class_ "bg-white rounded-lg border border-gray-200 p-5 shadow-sm"] $ do
                    div_ [class_ "text-sm font-medium text-gray-500 mb-1"] "Notes"
                    div_ [class_ "text-3xl font-bold text-gray-900"] $ toHtml (show (Map.size vault.notes) :: Text)

                -- Tasks section
                div_ $ do
                  forM_ (Map.toList groupedTasks) $ uncurry taskGroup
