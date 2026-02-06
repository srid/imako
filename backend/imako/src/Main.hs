{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.ByteString.Lazy qualified
import Data.LVar qualified as LVar
import Imako.API.WebSocket (wsApp)
import Imako.CLI qualified as CLI
import Imako.Core qualified as Core
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200, statusIsSuccessful)
import Network.Wai (Application, responseLBS, responseStatus)
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp qualified as Warp
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
  let indexHtmlPath = frontendPath </> "index.html"
  -- Configure static file settings (no SPA fallback here - handled by middleware)
  let settings =
        (defaultWebAppSettings $ fromString frontendPath)
          { ssIndices = [unsafeToPiece "index.html"]
          }
      staticFileApp = staticApp settings
      -- SPA fallback middleware: try static first, serve index.html for non-2xx
      spaFallback = spaMiddleware indexHtmlPath staticFileApp
      -- Handler is now pure: state -> query -> msg
      wsHandler = wsApp appStateVar (\st q -> pure $ Core.mkServerMessage vaultPath st q)
  pure $ websocketsOr WS.defaultConnectionOptions wsHandler spaFallback

-- | Middleware that serves static files first, falls back to index.html for non-2xx
spaMiddleware :: FilePath -> Application -> Application
spaMiddleware indexPath staticFileApp req respond = do
  -- Try static file first, intercept the response
  staticFileApp req $ \response -> do
    let status = responseStatus response
    if statusIsSuccessful status
      then respond response -- File found, serve it
      else do
        -- Non-2xx (404, 403, etc) - serve index.html for SPA routing
        indexContent <- readFileLBS indexPath
        respond $
          responseLBS
            status200
            [("Content-Type", "text/html; charset=utf-8")]
            indexContent

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    let url = "http://" <> options.host <> ":" <> show options.port
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    putTextLn $ "Starting server on " <> url

    Core.withAppState options.path $ \appStateVar -> do
      app <- mkApp options.path appStateVar

      let warpSettings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setPort options.port
              & Warp.setTimeout 600 -- 10 minutes (for long WebSocket connections)
      liftIO $ Warp.runSettings warpSettings app
