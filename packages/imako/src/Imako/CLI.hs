module Imako.CLI where

import Data.Char (toLower)

import Effectful.Colog.Simple (Severity (..))
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig, tlsConfigParser)
import Options.Applicative

-- Define the data structure to hold parsed arguments
data Options = Options
  { path :: FilePath
  , port :: Port
  , host :: Text
  , tlsConfig :: TLSConfig
  , logLevel :: Severity
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
    <*> tlsConfigParser
    <*> option
      (maybeReader parseSeverity)
      ( long "log-level"
          <> short 'l'
          <> metavar "LOG_LEVEL"
          <> value Info
          <> showDefault
          <> help "Log level (Debug, Info, Warning, Error)"
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

parseSeverity :: String -> Maybe Severity
parseSeverity s = case map toLower s of
  "debug" -> Just Debug
  "info" -> Just Info
  "warning" -> Just Warning
  "error" -> Just Error
  _ -> Nothing
