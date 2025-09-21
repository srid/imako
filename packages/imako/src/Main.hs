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
            head_ $ script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
            body_ [class_ "p-2 grid gap-2"] $ do
              titleBar $ do
                "Imako: "
                small_ $ code_ $ toHtml options.path
              let (pendingCount, completedCount, groupedTasks) = processTasksForUI (getTasks vault)
              div_ [class_ "text-right text-gray-600"] $
                toHtml (show pendingCount :: Text)
                  <> " pending, "
                  <> toHtml (show completedCount :: Text)
                  <> " completed, "
                  <> toHtml (show (Map.size vault.notes) :: Text)
                  <> " notes"

              -- Tasks section (displayed first)
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Tasks"
                forM_ (Map.toList groupedTasks) $ uncurry taskGroup

              -- Notes section
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Notes"
                div_ [class_ "font-mono"] $
                  forM_ (Map.toList vault.notes) $ \(key, _note) ->
                    div_ [class_ "p-2 border border-gray-300"] $ toHtml key
