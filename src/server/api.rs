//! Server function implementations.
use crate::shared::VaultInfo;
use dioxus::prelude::*;
use ob::folder_tree::build_folder_tree_from_notes;
use ob::{FolderNode, Note};
use std::path::PathBuf;

/// Global vault state, initialized once at startup.
#[allow(dead_code)]
static APP_STATE: std::sync::OnceLock<super::state::AppState> = std::sync::OnceLock::new();

/// Guard for starting the watcher exactly once (after tokio runtime is up).
#[allow(dead_code)]
static WATCHER_STARTED: std::sync::Once = std::sync::Once::new();

/// Initialize the app state. Call once at startup (before dioxus::launch).
/// NOTE: Does NOT start the watcher yet — tokio runtime isn't available here.
#[allow(dead_code)]
pub fn init(vault_root: PathBuf) {
  let state = super::state::AppState::new(vault_root);
  APP_STATE.set(state).expect("AppState already initialized");
}

/// Get app state and lazily start the watcher (first call after runtime is up).
#[allow(dead_code)]
pub fn app_state() -> &'static super::state::AppState {
  let state = APP_STATE.get().expect("AppState not initialized");
  WATCHER_STARTED.call_once(|| {
    state.start_watcher();
  });
  state
}

#[allow(dead_code)]
fn vault_info(state: &super::state::AppState) -> VaultInfo {
  let vault_name = state
    .vault_root
    .file_name()
    .map(|n| n.to_string_lossy().to_string())
    .unwrap_or_else(|| "Vault".to_string());
  let today = chrono::Local::now().date_naive();
  VaultInfo {
    vault_name,
    vault_path: state.vault_root.clone(),
    today,
  }
}

pub async fn get_vault_info_impl() -> Result<VaultInfo, ServerFnError> {
  let state = app_state();
  Ok(vault_info(state))
}

pub async fn get_folder_tree_impl() -> Result<FolderNode, ServerFnError> {
  let state = app_state();
  let vault = state.vault.read().await;
  Ok(build_folder_tree_from_notes(&vault.notes))
}

pub async fn get_note_impl(path: PathBuf) -> Result<Note, ServerFnError> {
  let state = app_state();
  let vault = state.vault.read().await;
  let note = vault
    .notes
    .get(&path)
    .ok_or_else(|| ServerFnError::new(format!("Note not found: {}", path.display())))?;
  Ok(note.clone())
}
