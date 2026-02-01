{-# LANGUAGE DeriveAnyClass #-}

{- | WebSocket protocol types for client-server communication.

Defines Query (from client) and Result (to client) types.
-}
module Imako.API.Protocol (
  Query (..),
  ServerMessage (..),
  QueryResponse (..),
  VaultInfo (..),
  TasksData (..),
  NotesData (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (Day)
import Imako.Core.FolderTree (FolderNode)

-- | Query sent from client to subscribe to data
data Query
  = -- | Subscribe to tasks view
    TasksQuery
  | -- | Subscribe to a specific note (path is vault-relative)
    NotesQuery FilePath
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Vault info included in all server messages
data VaultInfo = VaultInfo
  { vaultPath :: FilePath
  , vaultName :: Text
  , today :: Day
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Tasks-specific data
newtype TasksData = TasksData
  { folderTree :: FolderNode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Notes-specific data (rendered note content)
data NotesData = NotesData
  { notePath :: FilePath
  , noteHtml :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Query-specific response data (one variant per query type)
data QueryResponse
  = TasksResponse TasksData
  | NotesResponse NotesData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Server message sent to client (vault info + query-specific response)
data ServerMessage = ServerMessage
  { vaultInfo :: VaultInfo
  , response :: QueryResponse
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
