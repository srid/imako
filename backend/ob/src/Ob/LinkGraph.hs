{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- | Wikilink graph for tracking inter-note references.

Uses @algebraic-graphs@ 'AdjacencyMap' for efficient incremental updates
and O(log n) forward/backward link queries.

Edges represent resolved wikilinks: @source → target@.
-}
module Ob.LinkGraph (
  LinkGraph,

  -- * Graph construction
  buildNoteEdges,
  removeNoteEdges,

  -- * Queries
  backlinksOf,
  frontlinksOf,
) where

import Algebra.Graph.AdjacencyMap (AdjacencyMap)
import Algebra.Graph.AdjacencyMap qualified as AM
import Commonmark.Extensions.WikiLink qualified as WikiLink
import Data.IxSet.Typed qualified as Ix
import Data.Set qualified as Set
import Ob.Note (IxNote, Note (..))
import Text.Pandoc.Definition (Inline (..))
import Text.Pandoc.Walk (query)

-- | Directed graph of wikilink references between notes.
type LinkGraph = AdjacencyMap FilePath

-- | Extract resolved wikilink target paths from a note's Pandoc AST.
extractResolvedTargets :: IxNote -> Note -> [FilePath]
extractResolvedTargets notes note =
  ordNub $ mapMaybe resolve (extractWikilinkInlines note)
  where
    resolve :: Inline -> Maybe FilePath
    resolve inl
      | Just (wl, _) <- WikiLink.mkWikiLinkFromInline inl
      , Just target <- Ix.getOne (Ix.getEQ wl notes) =
          Just target.path
      | otherwise = Nothing

-- | Extract all wikilink Inline nodes from a note.
extractWikilinkInlines :: Note -> [Inline]
extractWikilinkInlines note = query go note.content
  where
    go :: Inline -> [Inline]
    go inl@Link {}
      | Just _ <- WikiLink.mkWikiLinkFromInline inl = [inl]
    go _ = []

-- | Build a star subgraph: note.path → [resolved targets]
buildNoteEdges :: IxNote -> Note -> LinkGraph
buildNoteEdges notes note =
  AM.star note.path (extractResolvedTargets notes note)

-- | Remove all edges from/to a note (for delete or pre-update).
removeNoteEdges :: FilePath -> LinkGraph -> LinkGraph
removeNoteEdges = AM.removeVertex

-- | Incremental update: remove old edges, overlay new ones.
updateNoteEdges :: IxNote -> Note -> LinkGraph -> LinkGraph
updateNoteEdges notes note graph =
  AM.overlay (buildNoteEdges notes note) (removeNoteEdges note.path graph)

-- | Build the full link graph from all notes.
buildLinkGraph :: IxNote -> LinkGraph
buildLinkGraph notes =
  AM.overlays $ map (buildNoteEdges notes) (Ix.toList notes)

-- | Get backlinks (notes that link TO this path). O(n · log n).
backlinksOf :: FilePath -> LinkGraph -> [FilePath]
backlinksOf path graph = sort . Set.toList $ AM.preSet path graph

-- | Get frontlinks (notes that this path links TO). O(log n).
frontlinksOf :: FilePath -> LinkGraph -> [FilePath]
frontlinksOf path graph = sort . Set.toList $ AM.postSet path graph
