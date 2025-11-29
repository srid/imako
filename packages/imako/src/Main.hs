{-# LANGUAGE OverloadedRecordDot #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Data.ByteString.Builder (Builder, lazyByteString)
import Data.LVar qualified as LVar
import Data.Time (Day, defaultTimeLocale, getZonedTime, localDay, parseTimeM, zonedTimeToLocalTime)
import Imako.CLI qualified as CLI
import Imako.Core (AppView (..), mkAppView)
import Imako.UI.DailyNoteInput (AppendResult (..), appendToDailyNote)
import Imako.UI.DailyNotes (renderDailyNoteWithSidebar, renderThisMoment)
import Imako.UI.Filters (renderFilterBar)
import Imako.UI.FolderTree (renderFolderTree)
import Imako.UI.Layout (layout)
import Imako.UI.PWA (imakoManifest)
import Imako.UI.Tasks (fileTreeItem)
import Imako.Web.Lucid (runAppHtml)
import Imako.Web.Static (mkStaticMiddleware)
import Lucid
import Main.Utf8 qualified as Utf8
import Network.HTTP.Types (status200, status400)
import Network.Wai (Application)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Handler.WarpTLS.Simple (TLSConfig (..), startWarpServer)
import Ob qualified
import Options.Applicative (execParser)
import System.FilePath ((</>))
import Web.Scotty qualified as S

renderMainContent :: (MonadReader AppView m) => HtmlT m ()
renderMainContent = do
  -- "This Moment" section - today's note + tasks due today
  renderThisMoment

  -- Filter Bar for tasks
  renderFilterBar

  -- Tasks section with hierarchical folder structure
  view <- ask
  renderFolderTree fileTreeItem view.folderTree

-- | Set up SSE headers and start streaming
sse :: ((Builder -> IO ()) -> IO () -> IO ()) -> S.ActionM ()
sse streamAction = do
  S.setHeader "Content-Type" "text/event-stream"
  S.setHeader "Cache-Control" "no-cache"
  S.setHeader "Connection" "keep-alive"
  S.status status200
  S.stream streamAction

-- | Create the Scotty application with all routes
mkApp :: FilePath -> LVar.LVar Ob.Vault -> IO Application
mkApp vaultPath vaultVar = do
  staticMiddleware <- mkStaticMiddleware
  app <- S.scottyApp $ do
    S.get "/" $ do
      vault <- liftIO $ LVar.get vaultVar
      today <- liftIO getLocalToday
      let view = mkAppView today vaultPath vault
          mainContent = toHtmlRaw $ runAppHtml view renderMainContent
      S.html $ renderText $ layout (toText vaultPath) mainContent

    S.post "/thought/add" $ do
      thought <- S.formParam "text"
      result <- liftIO $ appendToDailyNote vaultPath thought
      case result of
        Success -> S.text "OK"
        NoDailyNotesConfig -> do
          S.status status400
          S.text "Daily notes not configured in Obsidian"
        NoDailyNoteExists -> do
          S.status status400
          S.text "Today's daily note does not exist"

    -- HTMX endpoint for daily note content switching
    S.get "/daily/:day" $ do
      dayParam <- S.captureParam "day"
      case parseTimeM True defaultTimeLocale "%Y-%m-%d" dayParam of
        Nothing -> S.text "Invalid date format"
        Just selectedDay -> do
          vault <- liftIO $ LVar.get vaultVar
          today <- liftIO getLocalToday
          let view = mkAppView today vaultPath vault
          S.html $ runAppHtml view (renderDailyNoteWithSidebar selectedDay)

    S.get "/manifest.json" $ do
      S.setHeader "Content-Type" "application/json"
      S.json imakoManifest

    S.get "/events" $ do
      sse $ \write flush -> void $ infinitely $ do
        -- Wait for next update OR next midnight (to refresh dates)
        vault <-
          runConcurrently . asum . map Concurrently $
            [ listenDayChange >> LVar.get vaultVar
            , LVar.listenNext vaultVar
            ]
        today <- getLocalToday
        let view = mkAppView today vaultPath vault
            html = runAppHtml view renderMainContent
            sseData = "data: " <> html <> "\n\n"
        write $ lazyByteString $ encodeUtf8 sseData
        flush
  pure $ staticMiddleware app
  where
    -- \| Wait until the next midnight (local time)
    --
    -- Checks for day change every minute. This is simple and robust against system suspend.
    listenDayChange :: IO ()
    listenDayChange = do
      startDay <- getLocalToday
      fix $ \loop -> do
        threadDelay (60 * 1000000) -- 1 minute
        currentDay <- getLocalToday
        when (currentDay == startDay) loop

    -- \| Get the current day in the local timezone
    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

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
