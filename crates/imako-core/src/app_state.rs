//! Application state: vault + today + watchers.

use std::path::PathBuf;

use chrono::{Local, NaiveDate};
use ob::daily_notes;
use ob::vault::{self, Vault, VaultError};
use tokio::sync::watch;
use tracing::info;

/// Central application state.
#[derive(Debug, Clone)]
pub struct AppState {
    /// The loaded vault
    pub vault: Vault,
    /// Today's date
    pub today: NaiveDate,
    /// Path to today's daily note (if daily notes are configured)
    pub today_note_path: Option<String>,
}

impl AppState {
    /// Build app state from a vault.
    pub fn from_vault(vault: Vault) -> Self {
        let today = Local::now().date_naive();
        let today_note_path = vault
            .daily_notes_config
            .as_ref()
            .map(|config| daily_notes::get_today_note_path(config, today));

        AppState {
            vault,
            today,
            today_note_path,
        }
    }
}

/// Start the application with vault watching.
///
/// Returns a watch receiver that emits new `AppState` snapshots on vault changes.
pub async fn start_app(vault_path: PathBuf) -> Result<watch::Receiver<AppState>, VaultError> {
    let vault_rx = vault::watch_vault(vault_path).await?;

    let initial = AppState::from_vault(vault_rx.borrow().clone());
    let (tx, rx) = watch::channel(initial);

    // Forward vault updates → AppState updates
    tokio::spawn(async move {
        let mut vault_rx = vault_rx;
        while vault_rx.changed().await.is_ok() {
            let vault = vault_rx.borrow().clone();
            let state = AppState::from_vault(vault);
            info!("AppState updated: {} notes", state.vault.notes.len());
            let _ = tx.send(state);
        }
    });

    Ok(rx)
}
