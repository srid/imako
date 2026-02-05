module Imako.CLI where

import Network.Wai.Handler.Warp (Port)
import Options.Applicative

-- Define the data structure to hold parsed arguments
data Options = Options
  { path :: FilePath
  , port :: Port
  , host :: Text
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
    <*> option
      auto
      ( long "port"
          <> short 'p'
          <> metavar "PORT"
          <> value 4009
          <> showDefault
          <> help "Port to run the web server on"
      )
    <*> strOption
      ( long "host"
          <> short 'h'
          <> metavar "HOST"
          <> value "localhost"
          <> showDefault
          <> help "Host to bind the web server to"
      )

-- Parser info with additional configuration
opts :: ParserInfo Options
opts =
  info
    (optionsParser <**> helper)
    ( fullDesc
        <> header "Imako - Notebook Web Viewer"
        <> progDesc "Start a web server to view your notebook"
    )
