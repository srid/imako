//! Application state — holds the vault state and provides access to server functions.

use ob::vault::{scan_vault, VaultState, VaultWatcher};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;

/// Global application state shared across all server function calls.
#[derive(Debug)]
pub struct AppState {
    pub vault_root: PathBuf,
    pub vault: Arc<RwLock<VaultState>>,
}

#[allow(dead_code)]
impl AppState {
    /// Initialize application state by scanning the vault.
    pub fn new(vault_root: PathBuf) -> Self {
        let vault_state = scan_vault(&vault_root);
        Self {
            vault_root,
            vault: Arc::new(RwLock::new(vault_state)),
        }
    }

    /// Start the filesystem watcher in the background.
    pub fn start_watcher(&self) -> tokio::task::JoinHandle<()> {
        let watcher = VaultWatcher::new(self.vault_root.clone(), self.vault.clone());
        tokio::spawn(async move {
            if let Err(e) = watcher.watch().await {
                tracing::error!("Vault watcher error: {}", e);
            }
        })
    }
}
