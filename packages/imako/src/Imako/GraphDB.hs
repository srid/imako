{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans -Wno-missing-deriving-strategies #-}

module Imako.GraphDB (
  Node (..),
  GraphDB,
  emptyGraphDB,
  AddNode (..),
  AddEdge (..),
  GetAllNodes (..),
  GetAllEdges (..),
)
where

import Data.Acid (Query, Update, makeAcidic)
import Data.Map ()
import Data.Map qualified as Map
import Data.SafeCopy (SafeCopy, base, contain, deriveSafeCopy, getCopy, putCopy, safeGet, safePut)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import GHC.Generics ()

-- | A simple note with UUID and body text
data Node = Node
  { nodeId :: UUID
  , nodeBody :: Text
  }
  deriving stock (Show, Eq, Generic)

-- | Graph database with nodes and edges
data GraphDB = GraphDB
  { nodes :: Map UUID Node
  , edges :: [(UUID, UUID)]
  }
  deriving stock (Show, Eq, Generic)

emptyGraphDB :: GraphDB
emptyGraphDB = GraphDB mempty []

-- SafeCopy for UUID (orphan instance)
instance SafeCopy UUID where
  putCopy uuid = contain $ safePut (UUID.toText uuid)
  getCopy =
    contain
      ( safeGet
          >>= ( \case
                  Just uuid -> return uuid
                  Nothing -> fail "Invalid UUID"
              )
            . UUID.fromText
      )

$(deriveSafeCopy 0 'base ''Node)
$(deriveSafeCopy 0 'base ''GraphDB)

-- | Database operations
addNode :: Node -> Update GraphDB ()
addNode node = do
  db <- get
  put $ db {nodes = Map.insert (nodeId node) node (nodes db)}

addEdge :: UUID -> UUID -> Update GraphDB Bool
addEdge fromId toId = do
  db <- get
  if fromId `Map.member` nodes db && toId `Map.member` nodes db
    then do
      put $ db {edges = (fromId, toId) : edges db}
      return True
    else return False

getAllNodes :: Query GraphDB [Node]
getAllNodes = Map.elems . nodes <$> ask

getAllEdges :: Query GraphDB [(UUID, UUID)]
getAllEdges = edges <$> ask

$(makeAcidic ''GraphDB ['addNode, 'addEdge, 'getAllNodes, 'getAllEdges])
