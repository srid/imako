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
  WS.withPingThread conn 30 pass $ infinitely $ do
    vault <-
      runConcurrently . asum . map Concurrently $
        [ listenDayChange >> LVar.get vaultVar
        , LVar.listenNext vaultVar
        ]
    today <- getLocalToday
    let view = mkAppView today vaultPath vault
    WS.sendTextData conn (encode view)
  where
    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

    listenDayChange :: IO ()
    listenDayChange = do
      startDay <- getLocalToday
      fix $ \loop -> do
        threadDelay (60 * 1000000)
        currentDay <- getLocalToday
        when (currentDay == startDay) loop
