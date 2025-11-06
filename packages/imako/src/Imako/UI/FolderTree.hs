{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  FolderNode (..),
  buildFolderTree,
  renderFolderTree,
) where

import Data.Map.Strict qualified as Map
import Lucid
import System.FilePath (splitDirectories, (</>))
import Web.TablerIcons.Outline qualified as Icon

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
renderFolderTree :: FilePath -> (a -> Html ()) -> FolderNode a -> Html ()
renderFolderTree vaultPath renderItem rootNode =
  div_ $ renderFolderNode vaultPath renderItem "" rootNode

-- | Render a single folder node with all its contents
renderFolderNode :: FilePath -> (a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolderNode vaultPath renderItem currentPath node = do
  -- Render files in current folder first
  forM_ (Map.toList node.files) $
    uncurry (renderFileGroup vaultPath renderItem currentPath)

  -- Render subfolders below
  forM_ (Map.toList node.subfolders) $
    uncurry (renderFolder vaultPath renderItem currentPath)

-- | Render a collapsible folder
renderFolder :: FilePath -> (a -> Html ()) -> Text -> Text -> FolderNode a -> Html ()
renderFolder vaultPath renderItem parentPath folderName node = do
  let newPath = if parentPath == "" then folderName else parentPath <> "/" <> folderName
  details_ [class_ "mt-4 first:mt-0", open_ ""] $ do
    summary_ [class_ "cursor-pointer text-sm font-semibold text-gray-700 dark:text-gray-300 mb-2 px-4 py-2 bg-gray-100 dark:bg-gray-700 hover:bg-gray-200 dark:hover:bg-gray-600 rounded-lg border border-gray-300 dark:border-gray-600 flex items-center gap-2"] $ do
      div_ [class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center"] $ toHtmlRaw Icon.folder
      toHtml folderName
    -- Contents indented
    div_ [class_ "ml-4 mt-2"] $
      renderFolderNode vaultPath renderItem newPath node

-- | Render a file group within the hierarchy
renderFileGroup :: FilePath -> (a -> Html ()) -> Text -> Text -> a -> Html ()
renderFileGroup vaultPath renderItem currentPath filename item = do
  let relativePath = if currentPath == "" then filename else currentPath <> "/" <> filename
      absolutePath = vaultPath </> toString relativePath
      obsidianUrl = "obsidian://open?path=" <> toText absolutePath
  div_ [class_ "mt-4 first:mt-0"] $ do
    -- File header
    h3_ [class_ "text-sm font-semibold text-gray-600 dark:text-gray-300 mb-2 px-4 flex items-center gap-2 justify-between"] $ do
      div_ [class_ "flex items-center gap-2"] $ do
        div_ [class_ "w-3 h-3 flex-shrink-0 flex items-center justify-center"] $ toHtmlRaw Icon.file
        strong_ $ toHtml filename
      -- Edit link
      a_ [href_ obsidianUrl, class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center text-gray-500 dark:text-gray-400 hover:text-indigo-600 dark:hover:text-indigo-400 transition-colors", title_ "Edit in Obsidian"] $
        toHtmlRaw Icon.edit
    -- Item content
    div_ [class_ "bg-gray-50 dark:bg-gray-800 rounded-lg border border-gray-200 dark:border-gray-700 divide-y divide-gray-100 dark:divide-gray-700"] $
      renderItem item
