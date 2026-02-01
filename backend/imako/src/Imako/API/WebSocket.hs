{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

{- HLINT ignore "Use infinitely" -}

{- | WebSocket handler for real-time vault updates.

Receives Query from clients and pushes corresponding Results.
-}
module Imako.API.WebSocket (
  wsApp,
)
where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Concurrently (..), race_, runConcurrently)
import Control.Concurrent.STM qualified as STM
import Data.Aeson (decode, encode)
import Data.LVar qualified as LVar
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.API.Protocol qualified as Protocol
import Imako.Core qualified as Core
import Network.WebSockets qualified as WS
import Ob qualified

-- | Client state tracking current query subscription
newtype ClientState = ClientState
  { currentQuery :: Maybe Protocol.Query
  }

initialClientState :: ClientState
initialClientState = ClientState Nothing

-- | WebSocket server application that handles queries and pushes results
wsApp :: FilePath -> LVar.LVar Ob.Vault -> WS.ServerApp
wsApp vaultPath vaultVar pending = do
  conn <- WS.acceptRequest pending
  clientState <- STM.newTVarIO initialClientState

  WS.withPingThread conn 30 pass $ do
    -- Run message receiver and vault change listener concurrently
    -- Note: Using forever instead of infinitely because race_ expects IO ()
    race_
      (receiveMessages conn clientState)
      (listenForChanges conn clientState)
  where
    receiveMessages :: WS.Connection -> STM.TVar ClientState -> IO ()
    receiveMessages conn clientState = forever $ do
      msg <- WS.receiveData conn
      case decode msg of
        Just query -> do
          STM.atomically $ STM.writeTVar clientState (ClientState (Just query))
          sendResultForQuery conn query
        Nothing -> pass

    listenForChanges :: WS.Connection -> STM.TVar ClientState -> IO ()
    listenForChanges conn clientState = forever $ do
      void $
        runConcurrently . asum . map Concurrently $
          [ listenDayChange >> LVar.get vaultVar
          , LVar.listenNext vaultVar
          ]
      -- On change, send update for current query
      clientSt <- STM.readTVarIO clientState
      whenJust clientSt.currentQuery (sendResultForQuery conn)

    sendResultForQuery :: WS.Connection -> Protocol.Query -> IO ()
    sendResultForQuery conn query = do
      vault <- LVar.get vaultVar
      today <- getLocalToday
      let msg = mkServerMessage today vault query
      WS.sendTextData conn (encode msg)

    mkServerMessage :: Day -> Ob.Vault -> Protocol.Query -> Protocol.ServerMessage
    mkServerMessage today vault query =
      let vaultInfo = Core.mkVaultInfo vaultPath
       in case query of
            Protocol.TasksQuery ->
              Protocol.TasksResultMsg vaultInfo (Core.mkTasksData today vaultPath vault)
            Protocol.NotesQuery ->
              Protocol.NotesResultMsg vaultInfo (Core.mkNotesData vault)

    getLocalToday :: IO Day
    getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

    listenDayChange :: IO ()
    listenDayChange = do
      startDay <- getLocalToday
      fix $ \loop -> do
        threadDelay (60 * 1000000)
        currentDay <- getLocalToday
        when (currentDay == startDay) loop
