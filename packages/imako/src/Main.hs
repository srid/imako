{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Imako.CLI qualified as CLI
import Lucid
import Main.Utf8 qualified as Utf8
import Ob qualified
import Options.Applicative (execParser)
import Text.Pandoc.Definition (Pandoc)
import Web.Scotty qualified as S

type Note = Either Text (Maybe Aeson.Value, Pandoc)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    putTextLn "Starting web server on http://localhost:3000"
    Ob.withLiveNotebook options.path $ \notebookVar -> do
      S.scotty 3000 $ do
        S.get "/" $ do
          notebook <- liftIO $ readTVarIO notebookVar
          S.html $ renderText $ html_ $ body_ $ do
            h1_ $ "Imako (" <> toHtml options.path <> ")"
            p_ $ "Number of notes: " <> toHtml (show (Map.size notebook) :: Text)
