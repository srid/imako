{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | WebSocket protocol types for client-server communication.

Defines Query (from client) and Result (to client) types.
-}
module Imako.API.Protocol (
  Query (..),
  ServerMessage (..),
  QueryResponse (..),
  VaultInfo (..),
  VaultData (..),
  NotesData (..),
  protocolTsDeclarations,
)
where

import Data.Aeson (FromJSON, ToJSON, Value, defaultOptions)
import Data.Aeson.TypeScript.Internal (TSDeclaration)
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Data.Time (Day, UTCTime)
import Imako.Core.FolderTree (FolderNode)

-- | Day serializes as ISO date string (orphan instance, acceptable here)
instance TypeScript Day where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = [] -- No separate type needed, uses built-in string

-- | UTCTime serializes as ISO timestamp string
instance TypeScript UTCTime where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = []

-- | Query sent from client to subscribe to data
data Query
  = -- | Subscribe to vault view (folder tree with all files and tasks)
    VaultQuery
  | -- | Subscribe to a specific note (path is vault-relative)
    NotesQuery FilePath
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON)

$(deriveTypeScript defaultOptions ''Query)

-- | Vault info included in all server messages
data VaultInfo = VaultInfo
  { vaultPath :: FilePath
  , vaultName :: Text
  , today :: Day
  , notes :: Map Text UTCTime
  -- ^ All note paths (vault-relative) with last modified time
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''VaultInfo)

-- | Vault tree data (all files with their tasks)
newtype VaultData = VaultData
  { folderTree :: FolderNode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''VaultData)

-- | Notes-specific data (structured AST for client rendering)
data NotesData = NotesData
  { notePath :: FilePath
  , noteAst :: Value
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''NotesData)

-- | Query-specific response data (one variant per query type)
data QueryResponse
  = VaultResponse VaultData
  | NotesResponse NotesData
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''QueryResponse)

-- | Server message sent to client (vault info + query-specific response)
data ServerMessage = ServerMessage
  { vaultInfo :: VaultInfo
  , response :: QueryResponse
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

$(deriveTypeScript defaultOptions ''ServerMessage)

-- | All TypeScript declarations from this module
protocolTsDeclarations :: [TSDeclaration]
protocolTsDeclarations =
  mconcat
    [ getTypeScriptDeclarations (Proxy @Query)
    , getTypeScriptDeclarations (Proxy @VaultInfo)
    , getTypeScriptDeclarations (Proxy @VaultData)
    , getTypeScriptDeclarations (Proxy @NotesData)
    , getTypeScriptDeclarations (Proxy @QueryResponse)
    , getTypeScriptDeclarations (Proxy @ServerMessage)
    ]
