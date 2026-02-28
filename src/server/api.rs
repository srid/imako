//! Server function implementations.

use crate::shared::{FolderTreeData, NoteData, VaultInfo};
use dioxus::prelude::*;
use ob::folder_tree::build_folder_tree_from_notes;
use std::path::PathBuf;

/// Get the global vault state. For now, we use a once_cell.
/// In Phase 2 this will be replaced with proper Dioxus server context.
#[allow(dead_code)]
static APP_STATE: std::sync::OnceLock<super::state::AppState> = std::sync::OnceLock::new();

/// Initialize the app state. Call once at startup.
#[allow(dead_code)]
pub fn init(vault_root: PathBuf) {
    let state = super::state::AppState::new(vault_root);
    state.start_watcher();
    APP_STATE.set(state).expect("AppState already initialized");
}

#[allow(dead_code)]
fn app_state() -> &'static super::state::AppState {
    APP_STATE.get().expect("AppState not initialized")
}

#[allow(dead_code)]
fn vault_info(state: &super::state::AppState) -> VaultInfo {
    let vault_name = state
        .vault_root
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "Vault".to_string());
    let today = chrono::Local::now().format("%Y-%m-%d").to_string();

    VaultInfo {
        vault_name,
        vault_path: state.vault_root.to_string_lossy().to_string(),
        today,
    }
}

pub async fn get_folder_tree_impl() -> Result<FolderTreeData, ServerFnError> {
    let state = app_state();
    let vault = state.vault.read().await;
    let tree = build_folder_tree_from_notes(&vault.notes);
    let info = vault_info(state);

    Ok(FolderTreeData { info, tree })
}

pub async fn get_note_impl(path: String) -> Result<NoteData, ServerFnError> {
    let state = app_state();
    let vault = state.vault.read().await;
    let path_buf = PathBuf::from(&path);

    let note = vault
        .notes
        .get(&path_buf)
        .ok_or_else(|| ServerFnError::new(format!("Note not found: {}", path)))?;

    let info = vault_info(state);

    Ok(NoteData {
        info,
        note: note.clone(),
    })
}
