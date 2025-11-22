{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  FolderNode (..),
  buildFolderTree,
  renderFolderTree,
  folderStateScript,
)
where

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

{- | Render the entire folder tree as HTML.

Takes the root vault path, a function to render each file (given its path and associated data),
and the root 'FolderNode'. Renders all subfolders and files recursively.

@
renderFolderTree vaultPath renderFile rootNode
@

* @vaultPath@: The root path of the folder tree.
* @renderFile@: Function to render a file, given its path and associated data.
* @rootNode@: The root of the folder tree to render.

Returns an HTML representation of the folder tree.
-}
renderFolderTree :: FilePath -> (FilePath -> a -> Html ()) -> FolderNode a -> Html ()
renderFolderTree vaultPath renderFile rootNode =
  div_ [class_ "flex flex-col gap-0.5"] $ renderFolderNode vaultPath renderFile "" rootNode

-- | Render a single folder node with all its contents
renderFolderNode :: FilePath -> (FilePath -> a -> Html ()) -> Text -> FolderNode a -> Html ()
renderFolderNode vaultPath renderFile currentPath node = do
  -- Render subfolders first (usually looks better to have folders at top)
  forM_ (Map.toList node.subfolders) $
    uncurry (renderFolder vaultPath renderFile currentPath)

  -- Render files in current folder
  forM_ (Map.toList node.files) $
    uncurry (renderFileGroup vaultPath renderFile currentPath)

-- | Render a collapsible folder
renderFolder :: FilePath -> (FilePath -> a -> Html ()) -> Text -> Text -> FolderNode a -> Html ()
renderFolder vaultPath renderFile parentPath folderName node = do
  let newPath = if parentPath == "" then folderName else parentPath <> "/" <> folderName
      folderId = "folder-" <> sanitizeId newPath

  details_ [class_ "group/folder mt-3", open_ "", id_ folderId, term "data-folder-path" newPath] $ do
    summary_ [class_ "list-none cursor-pointer -mx-2 px-2 py-1 rounded bg-slate-600 dark:bg-gray-200 hover:bg-slate-500 dark:hover:bg-gray-300 flex items-center gap-2 text-sm font-medium select-none transition-colors text-white dark:text-gray-900"] $ do
      -- Chevron
      div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-400 dark:text-gray-600 transition-transform group-open/folder:rotate-90"] $
        toHtmlRaw Icon.chevron_right

      -- Icon
      div_ [class_ "text-gray-300 dark:text-gray-700"] $ toHtmlRaw Icon.folder

      -- Name
      toHtml folderName

    -- Contents (indented)
    div_ [class_ "pl-4 border-l border-gray-100 dark:border-gray-800 ml-2 mt-0.5 flex flex-col"] $
      renderFolderNode vaultPath renderFile newPath node

-- | Sanitize a path to create a valid HTML ID
sanitizeId :: Text -> Text
sanitizeId = toText . map sanitizeChar . toString
  where
    sanitizeChar c
      | c == '/' = '-'
      | c == ' ' = '_'
      | otherwise = c

-- | Render a file group within the hierarchy
renderFileGroup :: FilePath -> (FilePath -> a -> Html ()) -> Text -> Text -> a -> Html ()
renderFileGroup vaultPath renderFile currentPath filename item = do
  let relativePath = if currentPath == "" then filename else currentPath <> "/" <> filename
      absolutePath = vaultPath </> toString relativePath

  -- Delegate to the provided renderer (which should be fileTreeItem)
  renderFile absolutePath item

-- | JavaScript for persisting folder collapse/expand state in localStorage
folderStateScript :: Html ()
folderStateScript = do
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
      , "function restoreAndAttachListeners() {"
      , "  const states = loadFolderStates();"
      , "  "
      , "  // Restore saved states for folders"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    const path = details.getAttribute('data-folder-path');"
      , "    if (path in states) {"
      , "      details.open = states[path];"
      , "    }"
      , "    "
      , "    // Remove old listeners to avoid duplicates"
      , "    details.replaceWith(details.cloneNode(true));"
      , "  });"
      , "  "
      , "  // Re-query after replacing nodes and attach listeners"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    details.addEventListener('toggle', () => {"
      , "      const path = details.getAttribute('data-folder-path');"
      , "      saveFolderState(path, details.open);"
      , "    });"
      , "  });"
      , "}"
      , ""
      , "// Restore on initial page load"
      , "document.addEventListener('DOMContentLoaded', restoreAndAttachListeners);"
      , ""
      , "// Restore after htmx swaps content"
      , "document.addEventListener('htmx:afterSwap', restoreAndAttachListeners);"
      ]
