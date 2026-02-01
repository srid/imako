{- | WebSocket handler for real-time vault updates.

Pushes AppView to connected clients whenever the vault changes.
-}
module Imako.API.WebSocket (
  wsApp,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), runConcurrently)
import Data.Aeson (encode)
import Data.LVar qualified as LVar
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.Core (mkAppView)
import Network.WebSockets qualified as WS
import Ob qualified

-- | WebSocket server application that broadcasts vault changes
wsApp :: FilePath -> LVar.LVar Ob.Vault -> WS.ServerApp
wsApp vaultPath vaultVar pending = do
  conn <- WS.acceptRequest pending
  -- Send initial state immediately upon connection
  sendCurrentView conn
  -- Then listen for changes and push updates
  WS.withPingThread conn 30 pass $ void $ infinitely $ do
    void $
      runConcurrently . asum . map Concurrently $
        [ listenDayChange >> LVar.get vaultVar
        , LVar.listenNext vaultVar
        ]
    sendCurrentView conn
  where
    sendCurrentView :: WS.Connection -> IO ()
    sendCurrentView conn = do
      vault <- LVar.get vaultVar
      today <- getLocalToday
      let view = mkAppView today vaultPath vault
      WS.sendTextData conn (encode view)

    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

    listenDayChange :: IO ()
    listenDayChange = do
      startDay <- getLocalToday
      fix $ \loop -> do
        threadDelay (60 * 1000000)
        currentDay <- getLocalToday
        when (currentDay == startDay) loop
