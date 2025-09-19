{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.Map.Strict qualified as Map
import Imako.CLI qualified as CLI
import Imako.UI.Components (taskItem, titleBar)
import Lucid
import Main.Utf8 qualified as Utf8
import Ob (Notebook (..), Task (..))
import Ob qualified
import Ob.Task (extractText)
import Options.Applicative (execParser)
import Web.Scotty qualified as S

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:3000"
    Ob.withLiveNotebook options.path $ \notebookVar -> do
      S.scotty 3000 $ do
        S.get "/" $ do
          notebook <- liftIO $ readTVarIO notebookVar
          S.html $ renderText $ html_ $ do
            head_ $ script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)
            body_ [class_ "p-2 grid gap-2"] $ do
              titleBar $ do
                "Imako: "
                small_ $ code_ $ toHtml options.path
              div_ [class_ "text-right text-gray-600"] $
                toHtml (show (length (tasks notebook)) :: Text)
                  <> " tasks, "
                  <> toHtml (show (Map.size (notes notebook)) :: Text)
                  <> " notes"

              -- Tasks section (displayed first)
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Tasks"
                div_ $
                  forM_ (tasks notebook) $ \task ->
                    taskItem (extractText task.description) task.sourceNote task.isCompleted

              -- Notes section
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Notes"
                div_ [class_ "font-mono"] $
                  forM_ (Map.toList (notes notebook)) $ \(key, _note) ->
                    div_ [class_ "p-2 border border-gray-300"] $ toHtml key
