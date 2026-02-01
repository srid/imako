{-# LANGUAGE DeriveAnyClass #-}

{- | WebSocket protocol types for client-server communication.

Defines Query (from client) and Result (to client) types.
-}
module Imako.API.Protocol (
  Query (..),
  ServerMessage (..),
  VaultInfo (..),
  TasksData (..),
  NotesData (..),
)
where

import Data.Aeson (FromJSON, ToJSON)
import Data.Time (Day)
import Imako.Core.Filter (Filter)
import Imako.Core.FolderTree (FolderNode)

-- | Query sent from client to subscribe to data
data Query
  = -- | Subscribe to tasks view
    TasksQuery
  | -- | Subscribe to notes view
    NotesQuery
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

-- | Vault info included in all server messages
data VaultInfo = VaultInfo
  { vaultPath :: FilePath
  , vaultName :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Tasks-specific data (excludes vault info)
data TasksData = TasksData
  { folderTree :: FolderNode
  , filters :: [Filter]
  , today :: Day
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Notes-specific data
newtype NotesData = NotesData
  { noteCount :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

-- | Server message sent to client (always includes vault info)
data ServerMessage
  = TasksResultMsg VaultInfo TasksData
  | NotesResultMsg VaultInfo NotesData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
