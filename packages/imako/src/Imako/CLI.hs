module Imako.CLI where

import Options.Applicative

-- Define the data structure to hold parsed arguments
newtype Options = Options
  { path :: FilePath
  }
  deriving stock (Show)

-- Parser for the Options data structure
optionsParser :: Parser Options
optionsParser =
  Options
    <$> argument
      str
      ( metavar "PATH"
          <> help "Path to notebook"
      )

-- Parser info with additional configuration
opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Imako - Notebook Web Viewer"
        <> progDesc "Start a web server to view your notebook at http://localhost:4009"
    )
