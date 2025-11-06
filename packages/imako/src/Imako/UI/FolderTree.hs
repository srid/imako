{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  FolderNode (..),
  buildFolderTree,
  renderFolderTree,
  folderStateScript,
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
      folderId = "folder-" <> sanitizeId newPath
  details_ [class_ "mt-4 first:mt-0", open_ "", id_ folderId, term "data-folder-path" newPath] $ do
    summary_ [class_ "cursor-pointer text-sm font-semibold text-gray-800 dark:text-gray-100 mb-2 px-4 py-2 bg-indigo-50 dark:bg-indigo-900/30 hover:bg-indigo-100 dark:hover:bg-indigo-900/50 rounded-lg border border-indigo-200 dark:border-indigo-700 flex items-center gap-2"] $ do
      -- Chevron (collapsed by default, rotates when open)
      div_ [class_ "w-3 h-3 flex-shrink-0 flex items-center justify-center transition-transform chevron-icon"] $ toHtmlRaw Icon.chevron_right
      -- Folder icon
      div_ [class_ "w-4 h-4 flex-shrink-0 flex items-center justify-center"] $ toHtmlRaw Icon.folder
      toHtml folderName
    -- Contents indented
    div_ [class_ "ml-4 mt-2"] $
      renderFolderNode vaultPath renderItem newPath node

-- | Sanitize a path to create a valid HTML ID
sanitizeId :: Text -> Text
sanitizeId = toText . map sanitizeChar . toString
  where
    sanitizeChar c
      | c == '/' = '-'
      | c == ' ' = '_'
      | otherwise = c

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

-- | JavaScript for persisting folder collapse/expand state in localStorage
folderStateScript :: Html ()
folderStateScript = do
  -- CSS for chevron rotation
  style_ [] $
    unlines
      [ "details[open] > summary .chevron-icon {"
      , "  transform: rotate(90deg);"
      , "}"
      ]
  script_ [] $
    unlines
      [ "const STORAGE_KEY = 'imako-folder-states';"
      , ""
      , "function saveFolderState(path, isOpen) {"
      , "  const states = JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');"
      , "  states[path] = isOpen;"
      , "  localStorage.setItem(STORAGE_KEY, JSON.stringify(states));"
      , "}"
      , ""
      , "function loadFolderStates() {"
      , "  return JSON.parse(localStorage.getItem(STORAGE_KEY) || '{}');"
      , "}"
      , ""
      , "document.addEventListener('DOMContentLoaded', () => {"
      , "  const states = loadFolderStates();"
      , "  "
      , "  // Restore saved states"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    const path = details.getAttribute('data-folder-path');"
      , "    if (path in states) {"
      , "      details.open = states[path];"
      , "    }"
      , "  });"
      , "  "
      , "  // Listen for toggle events"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    details.addEventListener('toggle', () => {"
      , "      const path = details.getAttribute('data-folder-path');"
      , "      saveFolderState(path, details.open);"
      , "    });"
      , "  });"
      , "});"
      ]
