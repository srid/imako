{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  renderFolderTree,
)
where

import Data.Map.Strict qualified as Map
import Imako.Core.FolderTree (FolderNode (..))
import Lucid
import Ob (Task)
import Web.TablerIcons.Outline qualified as Icon

{- | Render the entire folder tree as HTML.

Takes the root vault path, a function to render each file (given vault path, relative path, and associated data),
and the root 'FolderNode'. Renders all subfolders and files recursively.

@
renderFolderTree vaultPath renderFile rootNode
@

* @vaultPath@: The root path of the folder tree.
* @renderFile@: Function to render a file, given vault path, relative path, and associated data.
* @rootNode@: The root of the folder tree to render.

Returns an HTML representation of the folder tree.
-}
renderFolderTree :: FilePath -> (FilePath -> FilePath -> [Task] -> Html ()) -> FolderNode -> Html ()
renderFolderTree vaultPath renderFile rootNode =
  div_ [class_ "flex flex-col gap-0.5"] $ renderFolderNode vaultPath renderFile "" rootNode

-- | Render a single folder node with all its contents
renderFolderNode :: FilePath -> (FilePath -> FilePath -> [Task] -> Html ()) -> Text -> FolderNode -> Html ()
renderFolderNode vaultPath renderFile currentPath node = do
  -- Render subfolders first (usually looks better to have folders at top)
  forM_ (Map.toList node.subfolders) $
    uncurry (renderFolder vaultPath renderFile currentPath)

  -- Render files in current folder
  forM_ (Map.toList node.files) $
    uncurry (renderFileGroup vaultPath renderFile currentPath)

-- | Render a collapsible folder
renderFolder :: FilePath -> (FilePath -> FilePath -> [Task] -> Html ()) -> Text -> Text -> FolderNode -> Html ()
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
renderFileGroup :: FilePath -> (FilePath -> FilePath -> [Task] -> Html ()) -> Text -> Text -> [Task] -> Html ()
renderFileGroup vaultPath renderFile currentPath filename tasks = do
  let relativePath = if currentPath == "" then filename else currentPath <> "/" <> filename

  -- Delegate to the provided renderer (which should be fileTreeItem)
  renderFile vaultPath (toString relativePath) tasks
