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

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.LVar qualified as LVar
import Network.WebSockets qualified as WS

-- | Client state tracking current query subscription
newtype ClientState q = ClientState
  { currentQuery :: Maybe q
  }

initialClientState :: ClientState q
initialClientState = ClientState Nothing

{- | Generic WebSocket server application.

Takes:
- An LVar with live state
- A handler function that builds responses from (state, query)
-}
wsApp ::
  (FromJSON q, ToJSON msg) =>
  LVar.LVar state ->
  (state -> q -> IO msg) ->
  WS.ServerApp
wsApp stateVar mkMessage pending = do
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
      void $ LVar.listenNext stateVar
      clientSt <- STM.readTVarIO clientState
      whenJust clientSt.currentQuery (sendResultForQuery conn)

    sendResultForQuery conn query = do
      st <- LVar.get stateVar
      msg <- mkMessage st query
      WS.sendTextData conn (encode msg)
