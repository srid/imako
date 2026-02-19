{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note (..),
  Task (..),
  TaskStatus (..),
  IxTask,
  Vault (..),
  IxNote,
  LinkGraph,
  DailyNote (..),
  DailyNotesConfig (..),
  getVault,
  getDailyNotes,
  backlinksOf,
  withLiveVault,
  getTodayNotePath,
  loadDailyNotesConfig,
  obsidianOpenUrl,
  noteToHtml,
)
where

import Ob.DailyNotes (DailyNote (..), DailyNotesConfig (..), getTodayNotePath, loadDailyNotesConfig)
import Ob.Html (noteToHtml)
import Ob.Link (obsidianOpenUrl)
import Ob.LinkGraph (LinkGraph, backlinksOf)
import Ob.Note (IxNote, Note (..))
import Ob.Task (IxTask, Task (..), TaskStatus (..))
import Ob.Vault (Vault (..), getDailyNotes, getVault, withLiveVault)
