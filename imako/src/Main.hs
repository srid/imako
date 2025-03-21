{-# LANGUAGE OverloadedRecordDot #-}

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
    if options.runOnce
      then void $ Ob.getNotebook options.path
      else Ob.withNotebook options.path $ \_notebook -> do
        pass
