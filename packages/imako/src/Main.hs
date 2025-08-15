{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}

module Main where

import Data.Aeson qualified as Aeson
import Imako.CLI qualified as CLI
import Main.Utf8 qualified as Utf8
import Ob qualified
import Options.Applicative (execParser)
import Text.Pandoc.Definition (Pandoc)

type Note = Either Text (Maybe Aeson.Value, Pandoc)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    case options.runOnce of
      True -> do
        _notebook <- Ob.getNotebook options.path
        pass
      False -> do
        Ob.withLiveNotebook options.path $ \_notebook -> do
          pass
