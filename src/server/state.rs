//! Application state — holds the vault state and event broadcast.
use ob::VaultEvent;
use ob::vault::{VaultState, VaultWatcher, scan_vault};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::{RwLock, broadcast};

/// Global application state shared across all server function calls.
#[derive(Debug)]
pub struct AppState {
  pub vault_root: PathBuf,
  pub vault: Arc<RwLock<VaultState>>,
  pub event_tx: broadcast::Sender<VaultEvent>,
}

#[allow(dead_code)]
impl AppState {
  /// Initialize application state by scanning the vault.
  pub fn new(vault_root: PathBuf) -> Self {
    let vault_state = scan_vault(&vault_root);
    let (event_tx, _) = broadcast::channel(256);
    Self {
      vault_root,
      vault: Arc::new(RwLock::new(vault_state)),
      event_tx,
    }
  }

  /// Subscribe to vault events (each client gets its own receiver).
  pub fn subscribe(&self) -> broadcast::Receiver<VaultEvent> {
    self.event_tx.subscribe()
  }

  /// Start the filesystem watcher in the background.
  pub fn start_watcher(&self) {
    let watcher = VaultWatcher::new(
      self.vault_root.clone(),
      self.vault.clone(),
      self.event_tx.clone(),
    );
    tokio::spawn(async move {
      if let Err(e) = watcher.watch().await {
        tracing::error!("Vault watcher error: {}", e);
      }
    });
  }
}
