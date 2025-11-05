{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  FolderNode (..),
  buildFolderTree,
  renderFolderTree,
) where

import Data.Map.Strict qualified as Map
import Lucid
import System.FilePath (splitDirectories)

-- | Hierarchical folder structure for organizing any type of data
data FolderNode a = FolderNode
  { subfolders :: Map Text (FolderNode a)
  , files :: Map Text a
  }
  deriving stock (Show)

-- | Build a folder tree from a map of file paths to items
buildFolderTree :: Map FilePath a -> FolderNode a
buildFolderTree = Map.foldlWithKey' insertFile emptyNode
  where
    emptyNode = FolderNode Map.empty Map.empty

    insertFile :: FolderNode a -> FilePath -> a -> FolderNode a
    insertFile node filepath item =
      let parts = splitDirectories filepath
       in insertPath node parts item

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

-- | Render the entire folder tree with a custom item renderer
renderFolderTree :: (a -> Html ()) -> FolderNode a -> Html ()
renderFolderTree renderItem rootNode =
  div_ $ renderFolderNode renderItem "" rootNode

-- | Render a single folder node with all its contents
renderFolderNode :: (a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolderNode renderItem _path node = do
  -- Render files in current folder first
  forM_ (Map.toList node.files) $ uncurry (renderFileGroup renderItem)

  -- Render subfolders below
  forM_ (Map.toList node.subfolders) $ uncurry (renderFolder renderItem)

-- | Render a collapsible folder
renderFolder :: (a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolder renderItem folderName node = do
  details_ [class_ "mt-4 first:mt-0", open_ ""] $ do
    summary_ [class_ "cursor-pointer text-sm font-semibold text-gray-700 mb-2 px-4 py-2 bg-gray-100 hover:bg-gray-200 rounded-lg border border-gray-300 flex items-center gap-2"] $ do
      span_ [class_ "text-base"] "📁"
      toHtml folderName
    -- Contents indented
    div_ [class_ "ml-4 mt-2"] $
      renderFolderNode renderItem folderName node

-- | Render a file group within the hierarchy
renderFileGroup :: (a -> Html ()) -> Text -> a -> Html ()
renderFileGroup renderItem filename item = do
  div_ [class_ "mt-4 first:mt-0"] $ do
    -- File header
    h3_ [class_ "text-xs font-semibold uppercase tracking-wider text-gray-500 mb-2 px-4 flex items-center gap-2"] $ do
      span_ [class_ "text-sm"] "📄"
      strong_ $ toHtml filename
    -- Item content
    div_ [class_ "bg-gray-50 rounded-lg border border-gray-200 divide-y divide-gray-100"] $
      renderItem item
