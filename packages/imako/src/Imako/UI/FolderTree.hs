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

{- | Render the entire folder tree with a custom item renderer
Note: renderItem is now expected to be compatible with fileTreeItem's expectations if we were passing it directly,
but here we are actually passing the *list of tasks* (type `a`) to `fileTreeItem`.
The `renderItem` argument is actually unused in the new design if we assume `a` is `[Task]`.
However, to keep the type signature generic as requested by the module structure, we might need to adjust.
Looking at `Main.hs` (not visible but inferred), `renderItem` was likely `taskGroup`.
We should probably change the type signature or usage.
For now, let's assume `a` is `[Task]` and we ignore `renderItem` because we use `fileTreeItem` directly?
No, `renderFolderTree` is generic. Let's look at how it's used.
Actually, `renderFileGroup` uses `renderItem`.
In the new design, `renderFileGroup` *is* `fileTreeItem` effectively.
So we should probably change `renderFolderTree` to take `(FilePath -> a -> Html ())` instead of just `(a -> Html ())`?
Or better, just let `renderItem` do the work of rendering the file node.
But `fileTreeItem` takes `Day`, `FilePath`, `[Task]`.
Let's assume the caller will pass a partially applied `fileTreeItem today`.
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

  details_ [class_ "group/folder", open_ "", id_ folderId, term "data-folder-path" newPath] $ do
    summary_ [class_ "list-none cursor-pointer -mx-2 px-2 py-1 rounded hover:bg-gray-100 dark:hover:bg-gray-800 flex items-center gap-2 text-sm font-medium select-none transition-colors text-gray-700 dark:text-gray-200"] $ do
      -- Chevron
      div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-400 transition-transform group-open/folder:rotate-90"] $
        toHtmlRaw Icon.chevron_right

      -- Icon
      div_ [class_ "text-blue-400 dark:text-blue-500"] $ toHtmlRaw Icon.folder

      -- Name
      toHtml folderName

    -- Contents (indented)
    div_ [class_ "pl-4 border-l border-gray-100 dark:border-gray-800 ml-2 mt-0.5 flex flex-col gap-0.5"] $
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
      , "document.addEventListener('DOMContentLoaded', () => {"
      , "  const states = loadFolderStates();"
      , "  "
      , "  // Restore saved states for folders"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    const path = details.getAttribute('data-folder-path');"
      , "    if (path in states) {"
      , "      details.open = states[path];"
      , "    }"
      , "  });"
      , "  "
      , "  // Listen for toggle events on folders"
      , "  document.querySelectorAll('details[data-folder-path]').forEach(details => {"
      , "    details.addEventListener('toggle', () => {"
      , "      const path = details.getAttribute('data-folder-path');"
      , "      saveFolderState(path, details.open);"
      , "    });"
      , "  });"
      , "});"
      ]
