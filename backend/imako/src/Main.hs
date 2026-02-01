{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.LVar qualified as LVar
import Imako.API.WebSocket (wsApp)
import Imako.CLI qualified as CLI
import Imako.Core qualified as Core
import Main.Utf8 qualified as Utf8
import Network.Wai (Application)
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Options.Applicative (execParser)
import System.FilePath ((</>))
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import WaiAppStatic.Types (ssIndices, unsafeToPiece)

-- | Create the WAI application with WebSocket and static files
mkApp :: FilePath -> LVar.LVar Core.AppState -> IO Application
mkApp vaultPath appStateVar = do
  -- Get frontend path from env var or fallback to frontend/dist for dev
  frontendPath <-
    lookupEnv "IMAKO_FRONTEND_PATH" >>= \case
      Just p -> pure p
      Nothing -> pure "frontend/dist"
  -- Configure static file settings to serve index.html for root
  let settings =
        (defaultWebAppSettings $ fromString frontendPath)
          { ssIndices = [unsafeToPiece "index.html"]
          }
      staticFileApp = staticApp settings
      -- Handler is now pure: state -> query -> msg
      wsHandler = wsApp appStateVar (\st q -> pure $ Core.mkServerMessage vaultPath st q)
  pure $ websocketsOr WS.defaultConnectionOptions wsHandler staticFileApp

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    let protocol = case options.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"
        url = protocol <> "://" <> options.host <> ":" <> show options.port
    putTextLn $ "Starting server on " <> url

    Core.withAppState options.path $ \appStateVar -> do
      app <- mkApp options.path appStateVar

      let warpSettings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setPort options.port
              & Warp.setTimeout 600 -- 10 minutes (for long WebSocket connections)
          tlsStateDir = options.path </> ".imako"
      liftIO $ startWarpServer warpSettings tlsStateDir options.tlsConfig app
