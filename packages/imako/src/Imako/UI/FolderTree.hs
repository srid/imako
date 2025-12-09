{-# LANGUAGE OverloadedRecordDot #-}

module Imako.UI.FolderTree (
  renderFolderTree,
)
where

import Data.Map.Strict qualified as Map
import Imako.Core (AppView (..))
import Imako.Core.FolderTree (FolderNode (..), hasDueTasks)
import Lucid
import Ob.Task (Task)
import Web.TablerIcons.Outline qualified as Icon

-- | Render a folder tree structure where each leaf is rendered by the provided function
renderFolderTree :: (MonadReader AppView m) => (FilePath -> [Task] -> HtmlT m ()) -> FolderNode -> HtmlT m ()
renderFolderTree renderLeaf node = do
  vaultPath <- asks (.vaultPath)
  div_ [class_ "flex flex-col gap-2"] $ renderFolderNode vaultPath renderLeaf "" node

-- | Render a single folder node with all its contents
renderFolderNode :: (MonadReader AppView m) => FilePath -> (FilePath -> [Task] -> HtmlT m ()) -> Text -> FolderNode -> HtmlT m ()
renderFolderNode vaultPath renderLeaf currentPath node = do
  -- Render subfolders first (usually looks better to have folders at top)
  forM_ (Map.toList node.subfolders) $
    uncurry (renderFolder vaultPath renderLeaf currentPath)

  -- Render files in this folder
  forM_ (Map.toList node.files) $
    uncurry (renderFileGroup vaultPath renderLeaf currentPath)

-- | Render a folder (directory) as a collapsible section
renderFolder :: (MonadReader AppView m) => FilePath -> (FilePath -> [Task] -> HtmlT m ()) -> Text -> Text -> FolderNode -> HtmlT m ()
renderFolder vaultPath renderLeaf currentPath folderName subNode = do
  let fullPath = if currentPath == "" then folderName else currentPath <> "/" <> folderName

  -- Check for due tasks
  today <- asks (.today)
  let hasDue = hasDueTasks today subNode

  details_ [class_ "group/folder", open_ "", term "data-folder-path" fullPath] $ do
    summary_ [class_ "list-none cursor-pointer -mx-2 px-2 py-1 rounded-md bg-slate-600 dark:bg-gray-200 hover:bg-slate-500 dark:hover:bg-gray-300 flex items-center gap-2 text-sm font-medium text-white dark:text-gray-900 select-none transition-colors"] $ do
      -- Chevron
      div_ [class_ "w-4 h-4 flex items-center justify-center text-gray-300 dark:text-gray-700 transition-transform group-open/folder:rotate-90"] $
        toHtmlRaw Icon.chevron_right

      -- Icon & Name
      div_ [class_ "flex items-center gap-1.5"] $ do
        div_ [class_ "text-gray-300 dark:text-gray-700"] $ toHtmlRaw Icon.folder
        span_ $ toHtml folderName

      -- Due indicator
      when hasDue $
        div_ [class_ "w-2 h-2 rounded-full bg-red-500 ml-auto mr-1", title_ "Contains due tasks"] mempty

    -- Folder contents (indented)
    div_ [class_ "pl-4 mt-2 flex flex-col gap-2"] $
      renderFolderNode vaultPath renderLeaf fullPath subNode

-- | Render a file group (tasks from a markdown file)
renderFileGroup :: (MonadReader AppView m) => FilePath -> (FilePath -> [Task] -> HtmlT m ()) -> Text -> Text -> [Task] -> HtmlT m ()
renderFileGroup _ renderLeaf currentPath filename tasks = do
  let relativePath = if currentPath == "" then filename else currentPath <> "/" <> filename
  renderLeaf (toString relativePath) tasks
