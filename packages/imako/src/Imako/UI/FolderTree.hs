{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  renderFolderTree,
)
where

import Data.Map.Strict qualified as Map
import Imako.Core.FolderTree (FolderNode (..))
import Lucid
import Ob.Task (Task)
import Web.TablerIcons.Outline qualified as Icon

-- | Render a folder tree structure where each leaf is rendered by the provided function
renderFolderTree :: FilePath -> (FilePath -> [Task] -> Html ()) -> FolderNode -> Html ()
renderFolderTree vaultPath renderLeaf node =
  div_ [class_ "flex flex-col gap-0.5"] $ renderFolderNode vaultPath renderLeaf "" node

-- | Render a single folder node with all its contents
renderFolderNode :: FilePath -> (FilePath -> [Task] -> Html ()) -> Text -> FolderNode -> Html ()
renderFolderNode vaultPath renderLeaf currentPath node = do
  -- Render subfolders first (usually looks better to have folders at top)
  forM_ (Map.toList node.subfolders) $
    uncurry (renderFolder vaultPath renderLeaf currentPath)

  -- Render files in this folder
  forM_ (Map.toList node.files) $
    uncurry (renderFileGroup vaultPath renderLeaf currentPath)

-- | Render a folder (directory) as a collapsible section
renderFolder :: FilePath -> (FilePath -> [Task] -> Html ()) -> Text -> Text -> FolderNode -> Html ()
renderFolder vaultPath renderLeaf currentPath folderName subNode = do
  let fullPath = if currentPath == "" then folderName else currentPath <> "/" <> folderName

  details_ [class_ "group/folder", open_ "", term "data-folder-path" fullPath] $ do
    summary_ [class_ "list-none cursor-pointer -mx-2 px-2 py-1 rounded-md hover:bg-gray-100 dark:hover:bg-gray-800 flex items-center gap-2 text-sm font-medium text-gray-700 dark:text-gray-300 select-none transition-colors"] $ do
      -- Chevron
      div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-400 dark:text-gray-600 transition-transform group-open/folder:rotate-90"] $
        toHtmlRaw Icon.chevron_right

      -- Icon & Name
      div_ [class_ "flex items-center gap-1.5"] $ do
        div_ [class_ "text-gray-400 dark:text-gray-600"] $ toHtmlRaw Icon.folder
        span_ $ toHtml folderName

    -- Folder contents (indented)
    div_ [class_ "pl-4"] $
      renderFolderNode vaultPath renderLeaf fullPath subNode

-- | Render a file group (tasks from a markdown file)
renderFileGroup :: FilePath -> (FilePath -> [Task] -> Html ()) -> Text -> Text -> [Task] -> Html ()
renderFileGroup _ renderLeaf currentPath filename tasks = do
  let relativePath = if currentPath == "" then filename else currentPath <> "/" <> filename
  renderLeaf (toString relativePath) tasks
