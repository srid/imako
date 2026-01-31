{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import Data.LVar qualified as LVar
import Imako.API (ImakoAPI, apiServer)
import Imako.API.WebSocket (wsApp)
import Imako.CLI qualified as CLI
import Main.Utf8 qualified as Utf8
import Network.Wai ()
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS
import Ob qualified
import Options.Applicative (execParser)
import Servant
import System.FilePath ((</>))

-- | Full API: JSON endpoints + static file serving
type FullAPI = ImakoAPI :<|> Raw

-- | Create the WAI application with API routes, WebSocket, and static files
mkApp :: FilePath -> LVar.LVar Ob.Vault -> IO Application
mkApp vaultPath vaultVar = do
  let api = apiServer vaultPath vaultVar
  -- Get frontend path from env var or fallback to frontend/dist for dev
  frontendPath <-
    lookupEnv "IMAKO_FRONTEND_PATH" >>= \case
      Just p -> pure p
      Nothing -> pure "frontend/dist"
  let static = serveDirectoryWebApp frontendPath
      app = serve (Proxy @FullAPI) (api :<|> static)
      wsHandler = wsApp vaultPath vaultVar
  pure $ websocketsOr WS.defaultConnectionOptions wsHandler app

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    let protocol = case options.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"
        url = protocol <> "://" <> options.host <> ":" <> show options.port
    putTextLn $ "Starting API server on " <> url
    Ob.withLiveVault options.path $ \vaultVar -> do
      app <- mkApp options.path vaultVar

      let settings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setPort options.port
              & Warp.setTimeout 600 -- 10 minutes (for long WebSocket connections)
          tlsStateDir = options.path </> ".imako"
      liftIO $ startWarpServer settings tlsStateDir options.tlsConfig app
