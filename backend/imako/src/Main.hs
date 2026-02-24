{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Data.LVar qualified as LVar
import Imako.API.Protocol (Query (..))
import Imako.API.WebSocket (wsApp)
import Imako.CLI qualified as CLI
import Imako.Core qualified as Core
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Network.Wai (Application, rawPathInfo, responseLBS)
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Options.Applicative (execParser)
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
  -- Configure static file settings (hash-based routing - no SPA fallback needed)
  let settings =
        (defaultWebAppSettings $ fromString frontendPath)
          { ssIndices = [unsafeToPiece "index.html"]
          }
      staticFileApp = staticApp settings
      -- Handler is now pure: state -> query -> msg
      -- querySlot groups queries by constructor for deduplication
      wsHandler = wsApp appStateVar querySlot (\st q -> pure $ Core.mkServerMessage vaultPath st q)
      app = healthMiddleware $ websocketsOr WS.defaultConnectionOptions wsHandler staticFileApp
  pure app

{- | Slot key for query deduplication.
Queries with the same slot replace each other; different slots coexist.
-}
querySlot :: Query -> Text
querySlot FolderTreeQuery = "vault"
querySlot (NotesQuery _) = "notes"

-- | Middleware that responds to /health with 200 OK (for readiness probes)
healthMiddleware :: Application -> Application
healthMiddleware backendApp req respond
  | rawPathInfo req == "/health" = respond $ responseLBS status200 [] "ok"
  | otherwise = backendApp req respond

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering

    Core.withAppState options.path $ \appStateVar -> do
      app <- mkApp options.path appStateVar

      let baseSettings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setTimeout 600 -- 10 minutes (for long WebSocket connections)
      if options.port == 0
        then do
          -- Auto-select a free port
          (actualPort, sock) <- Warp.openFreePort
          let url = "http://" <> options.host <> ":" <> show actualPort
          putTextLn $ "Starting server on " <> url
          let settings = baseSettings & Warp.setPort actualPort
          liftIO $ Warp.runSettingsSocket settings sock app
        else do
          let url = "http://" <> options.host <> ":" <> show options.port
          putTextLn $ "Starting server on " <> url
          let settings = baseSettings & Warp.setPort options.port
          liftIO $ Warp.runSettings settings app
