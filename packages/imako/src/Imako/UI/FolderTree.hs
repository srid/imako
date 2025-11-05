{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  FolderNode (..),
  buildFolderTree,
  renderFolderTree,
) where

import Data.Map.Strict qualified as Map
import Imako.UI.Components (taskItem)
import Lucid
import Ob.Task (Task (..))
import System.FilePath (splitDirectories)

-- | Hierarchical folder structure for organizing tasks
data FolderNode = FolderNode
  { subfolders :: Map Text FolderNode
  , files :: Map Text [Task] -- filename -> tasks
  }
  deriving stock (Show)

-- | Build a folder tree from a map of file paths to tasks
buildFolderTree :: Map FilePath [Task] -> FolderNode
buildFolderTree = Map.foldlWithKey' insertFile emptyNode
  where
    emptyNode = FolderNode Map.empty Map.empty

    insertFile :: FolderNode -> FilePath -> [Task] -> FolderNode
    insertFile node filepath tasks =
      let parts = splitDirectories filepath
       in insertPath node parts tasks

    insertPath :: FolderNode -> [FilePath] -> [Task] -> FolderNode
    insertPath node [] _ = node -- shouldn't happen
    insertPath node [filename] tasks =
      -- Leaf: add file to current node
      node {files = Map.insert (toText filename) tasks node.files}
    insertPath node (folder : rest) tasks =
      -- Branch: recurse into subfolder
      let subfolderName = toText folder
          existingSubfolder = Map.findWithDefault emptyNode subfolderName node.subfolders
          updatedSubfolder = insertPath existingSubfolder rest tasks
       in node {subfolders = Map.insert subfolderName updatedSubfolder node.subfolders}

-- | Render the entire folder tree
renderFolderTree :: FolderNode -> Html ()
renderFolderTree rootNode =
  div_ $ renderFolderNode "" rootNode

-- | Render a single folder node with all its contents
renderFolderNode :: Text -> FolderNode -> Html ()
renderFolderNode _path node = do
  -- Render files in current folder first
  forM_ (Map.toList node.files) $ uncurry renderFileGroup

  -- Render subfolders below
  forM_ (Map.toList node.subfolders) $ uncurry renderFolder

-- | Render a collapsible folder
renderFolder :: Text -> FolderNode -> Html ()
renderFolder folderName node = do
  details_ [class_ "mt-4 first:mt-0", open_ ""] $ do
    summary_ [class_ "cursor-pointer text-sm font-semibold text-gray-700 mb-2 px-4 py-2 bg-gray-100 hover:bg-gray-200 rounded-lg border border-gray-300 flex items-center gap-2"] $ do
      span_ [class_ "text-base"] "📁"
      toHtml folderName
    -- Contents indented
    div_ [class_ "ml-4 mt-2"] $
      renderFolderNode folderName node

-- | Render a file group within the hierarchy
renderFileGroup :: Text -> [Task] -> Html ()
renderFileGroup filename tasks = do
  div_ [class_ "mt-4 first:mt-0"] $ do
    -- File header
    h3_ [class_ "text-xs font-semibold uppercase tracking-wider text-gray-500 mb-2 px-4 flex items-center gap-2"] $ do
      span_ [class_ "text-sm"] "📄"
      strong_ $ toHtml filename
    -- Tasks
    div_ [class_ "bg-gray-50 rounded-lg border border-gray-200 divide-y divide-gray-100"] $
      forM_ tasks taskItem
