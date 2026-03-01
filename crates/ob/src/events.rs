//! Vault change events for live sync.
use crate::Note;
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// A change event produced by the vault watcher.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum VaultEvent {
  /// A note was modified (re-parsed content).
  NoteModified { path: PathBuf, note: Note },

  /// A new note was added to the vault.
  NoteAdded { path: PathBuf, note: Note },

  /// A note was deleted from the vault.
  NoteDeleted { path: PathBuf },

  /// The day boundary was crossed (midnight).
  DayChanged { today: chrono::NaiveDate },
}
