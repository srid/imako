{-# OPTIONS_GHC -Wno-x-partial #-}

{- HLINT ignore "Use infinitely" -}

{- | Generic WebSocket handler for real-time data updates.

Receives queries from clients and pushes corresponding results.
Supports multiple concurrent subscriptions: when the model changes,
responses are sent for ALL active queries, not just the last one.

Queries are deduplicated by a user-supplied key function, so only
the most recent query per "slot" is kept (e.g. one vault subscription
and one notes subscription).
-}
module Imako.API.WebSocket (
  wsApp,
)
where

import Control.Concurrent.Async (race_)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import Network.WebSockets qualified as WS

{- | Generic WebSocket server application.

Takes:
- An LVar with live state
- A key function to deduplicate queries (same key = replace)
- A handler function that builds responses from (state, query)

On model change, sends a response for EACH active query slot,
enabling the client to subscribe to multiple data streams.
-}
wsApp ::
  (FromJSON q, ToJSON msg, Ord k) =>
  LVar.LVar state ->
  (q -> k) ->
  (state -> q -> IO msg) ->
  WS.ServerApp
wsApp stateVar queryKey mkMessage pending = do
  conn <- WS.acceptRequest pending
  -- One query per key slot (newest wins)
  activeQueries <- STM.newTVarIO (Map.empty :: Map.Map k q)

  WS.withPingThread conn 30 pass $ do
    race_
      (receiveMessages conn activeQueries)
      (listenForChanges conn activeQueries)
  where
    receiveMessages conn activeQueries = forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Just query -> do
          STM.atomically $
            STM.modifyTVar' activeQueries (Map.insert (queryKey query) query)
          sendResultForQuery conn query
        Nothing -> pass

    listenForChanges conn activeQueries = forever $ do
      void $ LVar.listenNext stateVar
      queries <- Map.elems <$> STM.readTVarIO activeQueries
      forM_ queries (sendResultForQuery conn)

    sendResultForQuery conn query = do
      st <- LVar.get stateVar
      msg <- mkMessage st query
      WS.sendTextData conn (encode msg)
