{-|
Module      : Imako.CLI
Description : Command-line interface for Imako
Copyright   : (c) 2024 Sridhar Ratnakumar
License     : AGPL-3.0-or-later
Maintainer  : srid@srid.ca

This module provides the command-line interface for Imako,
a journaling and planning tool for Obsidian notebooks.
-}
module Imako.CLI (
  -- * Types
  Options (..),
  -- * Parsers
  opts,
) where

import Options.Applicative

-- | Command-line options for Imako.
-- Holds the parsed arguments from the command line.
newtype Options = Options
  { path :: FilePath
  -- ^ Path to the Obsidian notebook directory
  }
  deriving stock (Show)

-- | Parser for the Options data structure.
-- Defines how command-line arguments are parsed into Options.
optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument
      str
      ( metavar "PATH"
          <> help "Path to notebook"
      )

-- | Parser info with additional configuration.
-- This defines the full command-line interface including help text and description.
opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Imako - Notebook Web Viewer"
        <> progDesc "Start a web server to view your notebook at http://localhost:3000"
    )
