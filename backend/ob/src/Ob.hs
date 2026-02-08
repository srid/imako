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
  DailyNote (..),
  DailyNotesConfig (..),
  getVault,
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
import Ob.Note (IxNote, Note (..))
import Ob.Task (IxTask, Task (..), TaskStatus (..))
import Ob.Vault (Vault (..), getVault, withLiveVault)
