{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{- HLINT ignore "Use infinitely" -}

{- | Generic WebSocket handler for real-time data updates.

Receives queries from clients and pushes corresponding results.
The response building is passed in as a handler function.
-}
module Imako.API.WebSocket (
  wsApp,
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), race_, runConcurrently)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.LVar qualified as LVar
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Network.WebSockets qualified as WS

-- | Client state tracking current query subscription
newtype ClientState q = ClientState
  { currentQuery :: Maybe q
  }

initialClientState :: ClientState q
initialClientState = ClientState Nothing

{- | Generic WebSocket server application.

Takes:
- An LVar with live data
- A handler function that builds responses given (Day, vault, query)
-}
wsApp ::
  (FromJSON q, ToJSON msg) =>
  LVar.LVar vault ->
  (Day -> vault -> q -> msg) ->
  WS.ServerApp
wsApp vaultVar mkMessage pending = do
  conn <- WS.acceptRequest pending
  clientState <- STM.newTVarIO initialClientState

  WS.withPingThread conn 30 pass $ do
    race_
      (receiveMessages conn clientState)
      (listenForChanges conn clientState)
  where
    receiveMessages conn clientState = forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Just query -> do
          STM.atomically $ STM.writeTVar clientState (ClientState (Just query))
          sendResultForQuery conn query
        Nothing -> pass

    listenForChanges conn clientState = forever $ do
      void $
        runConcurrently . asum . map Concurrently $
          [ listenDayChange >> LVar.get vaultVar
          , LVar.listenNext vaultVar
          ]
      clientSt <- STM.readTVarIO clientState
      whenJust clientSt.currentQuery (sendResultForQuery conn)

    sendResultForQuery conn query = do
      vault <- LVar.get vaultVar
      today <- getLocalToday
      let msg = mkMessage today vault query
      WS.sendTextData conn (encode msg)

    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

    listenDayChange = do
      startDay <- getLocalToday
      fix $ \loop -> do
        threadDelay (60 * 1000000)
        currentDay <- getLocalToday
        when (currentDay == startDay) loop
