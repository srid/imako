{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infinitely" #-}

module Main where

import Data.ByteString.Builder (lazyByteString)
import Data.LVar qualified as LVar
import Data.Text.Lazy.Encoding qualified as TL
import Data.Time (Day, getCurrentTime, getCurrentTimeZone, localDay, utcToLocalTime)
import Imako.CLI qualified as CLI
import Imako.Core (AppView (..), mkAppView)
import Imako.UI.Filters (renderFilterBar)
import Imako.UI.FolderTree (renderFolderTree)
import Imako.UI.Inbox (appendToInbox)
import Imako.UI.Layout (layout)
import Imako.UI.PWA (imakoManifest)
import Imako.UI.Tasks (fileTreeItem)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Ob qualified
import Options.Applicative (execParser)
import System.FilePath ((</>))
import Web.Scotty qualified as S

renderMainContent :: Day -> FilePath -> Ob.Vault -> Html ()
renderMainContent today vaultPath vault = do
  let view = mkAppView vaultPath vault

  -- Filter Bar
  renderFilterBar view.filters

  -- Tasks section with hierarchical folder structure
  renderFolderTree vaultPath (fileTreeItem view.filters today) view.folderTree

-- | Create the Scotty application with all routes
mkApp :: FilePath -> LVar.LVar Ob.Vault -> IO Application
mkApp vaultPath vaultVar = S.scottyApp $ do
  S.get "/" $ do
    today <- liftIO getLocalToday
    vault <- liftIO $ LVar.get vaultVar
    S.html $
      renderText $
        layout (toText vaultPath) (renderMainContent today vaultPath vault)

  S.post "/inbox/add" $ do
    taskText <- S.formParam "text"
    liftIO $ appendToInbox vaultPath taskText
    S.text "OK"

  S.get "/manifest.json" $ do
    S.setHeader "Content-Type" "application/json"
    S.json imakoManifest

  S.get "/events" $ do
    S.setHeader "Content-Type" "text/event-stream"
    S.setHeader "Cache-Control" "no-cache"
    S.setHeader "Connection" "keep-alive"
    S.status status200
    S.stream $ \write flush -> forever $ do
      today <- getLocalToday
      vault <- LVar.listenNext vaultVar
      let html = renderText $ renderMainContent today vaultPath vault
      let sseData = "data: " <> html <> "\n\n"
      write $ lazyByteString $ TL.encodeUtf8 sseData
      flush
  where
    -- \| Get the current day in the local timezone
    getLocalToday :: IO Day
    getLocalToday = do
      now <- getCurrentTime
      tz <- getCurrentTimeZone
      pure $ localDay (utcToLocalTime tz now)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    options <- liftIO $ execParser CLI.opts
    let protocol = case options.tlsConfig of
          TLSDisabled -> "http"
          _ -> "https"
        url = protocol <> "://" <> options.host <> ":" <> show options.port
    putTextLn $ "Starting web server on " <> url
    Ob.withLiveVault options.path $ \vaultVar -> do
      app <- mkApp options.path vaultVar

      let settings =
            Warp.defaultSettings
              & Warp.setHost (fromString $ toString options.host)
              & Warp.setPort options.port
              & Warp.setTimeout 600 -- 10 minutes (for long SSE connections)
          tlsStateDir = options.path </> ".imako"
      liftIO $ startWarpServer settings tlsStateDir options.tlsConfig app
