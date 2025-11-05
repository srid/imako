{-# LANGUAGE OverloadedRecordDot #-}

{-|
Module      : Imako.UI.FolderTree
Description : Hierarchical folder tree structure for organizing content
Copyright   : (c) 2024 Sridhar Ratnakumar
License     : AGPL-3.0-or-later
Maintainer  : srid@srid.ca

This module provides a hierarchical folder tree data structure that can organize
any type of data by file paths. It's used in Imako to display tasks grouped by
their source files in a collapsible folder hierarchy.
-}
module Imako.UI.FolderTree (
  -- * Data Types
  FolderNode (..),
  -- * Building Trees
  buildFolderTree,
  -- * Rendering Trees
  renderFolderTree,
) where

import Data.Map.Strict qualified as Map
import Lucid
import System.FilePath (splitDirectories)
import Web.TablerIcons.Outline qualified as Icon

-- | Hierarchical folder structure for organizing any type of data.
-- Each node contains a map of subfolders and a map of files.
-- The type parameter @a@ represents the data associated with each file.
data FolderNode a = FolderNode
  { subfolders :: Map Text (FolderNode a)
  -- ^ Map of subfolder names to their corresponding folder nodes
  , files :: Map Text a
  -- ^ Map of file names to their associated data
  }
  deriving stock (Show)

-- | Build a folder tree from a map of file paths to items.
-- Takes a map where keys are file paths and values are the data to associate with each file.
-- The file paths are split into directory components and organized into a hierarchical structure.
--
-- ==== __Example__
--
-- > buildFolderTree (Map.fromList [("docs/README.md", content1), ("src/Main.hs", content2)])
--
-- This creates a tree with two folders: @docs@ and @src@, each containing one file.
buildFolderTree :: Map FilePath a -> FolderNode a
buildFolderTree = Map.foldlWithKey' insertFile emptyNode
  where
    -- | An empty folder node with no subfolders or files
    emptyNode :: FolderNode a
    emptyNode = FolderNode Map.empty Map.empty

    -- | Insert a single file into the folder tree
    insertFile :: FolderNode a -> FilePath -> a -> FolderNode a
    insertFile node filepath item =
      let parts = splitDirectories filepath
       in insertPath node parts item

    -- | Insert a file into the tree by following a path of directory names
    insertPath :: FolderNode a -> [FilePath] -> a -> FolderNode a
    insertPath node [] _ = node -- shouldn't happen
    insertPath node [filename] item =
      -- Leaf: add file to current node
      node {files = Map.insert (toText filename) item node.files}
    insertPath node (folder : rest) item =
      -- Branch: recurse into subfolder
      let subfolderName = toText folder
          existingSubfolder = Map.findWithDefault emptyNode subfolderName node.subfolders
          updatedSubfolder = insertPath existingSubfolder rest item
       in node {subfolders = Map.insert subfolderName updatedSubfolder node.subfolders}

-- | Render the entire folder tree with a custom item renderer.
-- Takes a function that knows how to render the data associated with each file,
-- and applies it to all files in the tree while maintaining the folder hierarchy.
--
-- ==== __Parameters__
--
-- * @renderItem@ - Function to render the data associated with each file
-- * @rootNode@ - The root node of the folder tree to render
renderFolderTree :: (a -> Html ()) -> FolderNode a -> Html ()
renderFolderTree renderItem rootNode =
  div_ $ renderFolderNode renderItem "" rootNode

-- | Render a single folder node with all its contents.
-- Renders files first, then subfolders. This is a recursive function that
-- handles the tree traversal.
--
-- ==== __Parameters__
--
-- * @renderItem@ - Function to render the data associated with each file
-- * @_path@ - Current path in the tree (for future use, currently unused)
-- * @node@ - The folder node to render
renderFolderNode :: (a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolderNode renderItem _path node = do
  -- Render files in current folder first
  forM_ (Map.toList node.files) $ uncurry (renderFileGroup renderItem)

  -- Render subfolders below
  forM_ (Map.toList node.subfolders) $ uncurry (renderFolder renderItem)

-- | Render a collapsible folder.
-- Creates a details/summary HTML structure for a folder that can be expanded
-- or collapsed. The folder contains all its files and subfolders.
--
-- ==== __Parameters__
--
-- * @renderItem@ - Function to render the data associated with each file
-- * @folderName@ - The name of the folder to display
-- * @node@ - The folder node containing the folder's contents
renderFolder :: (a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolder renderItem folderName node = do
  details_ [class_ "mt-4 first:mt-0", open_ ""] $ do
    summary_ [class_ "cursor-pointer text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2 px-4 py-2 bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 rounded-lg border border-gray-300 dark:border-gray-600 flex items-center gap-2"] $ do
      div_ [class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center"] $ toHtmlRaw Icon.folder
      toHtml folderName
    -- Contents indented
    div_ [class_ "ml-4 mt-2"] $
      renderFolderNode renderItem folderName node

-- | Render a file group within the hierarchy.
-- Displays a file header with its name and icon, followed by the rendered
-- content of the file's associated data.
--
-- ==== __Parameters__
--
-- * @renderItem@ - Function to render the data associated with the file
-- * @filename@ - The name of the file to display
-- * @item@ - The data associated with this file
renderFileGroup :: (a -> Html ()) -> Text -> a -> Html ()
renderFileGroup renderItem filename item = do
  div_ [class_ "mt-4 first:mt-0"] $ do
    -- File header
    h3_ [class_ "text-sm font-semibold text-gray-600 dark:text-gray-300 mb-2 px-4 flex items-center gap-2"] $ do
      div_ [class_ "w-3 h-3 flex-shrink-0 flex items-center justify-center"] $ toHtmlRaw Icon.file
      strong_ $ toHtml filename
    -- Item content
    div_ [class_ "bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 divide-y divide-gray-100 dark:divide-gray-700"] $
      renderItem item
