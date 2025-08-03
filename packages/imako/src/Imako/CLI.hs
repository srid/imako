module Imako.CLI where

import Options.Applicative

-- Define the data structure to hold parsed arguments
data Options = Options
  { runOnce :: Bool
  , path :: FilePath
  }
  deriving stock (Show)

-- Parser for the Options data structure
optionsParser :: Parser Options
optionsParser =
  Options
    <$> switch
      ( long "run-once"
          <> help "Run the program once and exit"
      )
    <*> argument
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
        <> header "Imako"
    )
