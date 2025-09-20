{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.Map.Strict qualified as Map
import Imako.CLI qualified as CLI
import Imako.UI.Components (taskItem, titleBar)
import Lucid
import Main.Utf8 qualified as Utf8
import Ob qualified
import Ob.Vault (getTasks)
import Options.Applicative (execParser)
import Web.Scotty qualified as S

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
              div_ [class_ "text-right text-gray-600"] $
                toHtml (show (length (getTasks vault)) :: Text)
                  <> " tasks, "
                  <> toHtml (show (Map.size vault.notes) :: Text)
                  <> " notes"

              -- Tasks section (displayed first)
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Tasks"
                div_ $
                  forM_ (getTasks vault) $ \task ->
                    taskItem task

              -- Notes section
              div_ $ do
                h2_ [class_ "text-lg font-semibold mb-2"] "Notes"
                div_ [class_ "font-mono"] $
                  forM_ (Map.toList vault.notes) $ \(key, _note) ->
                    div_ [class_ "p-2 border border-gray-300"] $ toHtml key
