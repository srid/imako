//! Shared protocol types for server-client communication.

use chrono::NaiveDate;
use ob::daily_notes::DailyNote;
use ob::task::Task;
use serde::{Deserialize, Serialize};

use crate::folder_tree::FolderNode;

/// Vault metadata sent to the client.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VaultInfo {
    pub vault_name: String,
    pub today: NaiveDate,
    pub daily_notes_folder: Option<String>,
    pub today_note_path: Option<String>,
}

/// Data for rendering the vault tree view.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct VaultTreeData {
    pub info: VaultInfo,
    pub tree: FolderNode,
    pub daily_notes: Vec<DailyNote>,
}

/// Data for rendering a single note.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct NoteData {
    pub path: String,
    pub html: String,
    pub backlinks: Vec<String>,
    pub tasks: Vec<Task>,
}

/// A server push message for live updates.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum ServerMessage {
    /// Full vault tree update
    VaultUpdate(VaultTreeData),
}
