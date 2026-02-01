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
  currentQuery <- STM.newTVarIO (Nothing :: Maybe q)

  WS.withPingThread conn 30 pass $ do
    race_
      (receiveMessages conn currentQuery)
      (listenForChanges conn currentQuery)
  where
    receiveMessages conn currentQuery = forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Just query -> do
          STM.atomically $ STM.writeTVar currentQuery (Just query)
          sendResultForQuery conn query
        Nothing -> pass

    listenForChanges conn currentQuery = forever $ do
      void $ LVar.listenNext stateVar
      mQuery <- STM.readTVarIO currentQuery
      whenJust mQuery (sendResultForQuery conn)

    sendResultForQuery conn query = do
      st <- LVar.get stateVar
      msg <- mkMessage st query
      WS.sendTextData conn (encode msg)
