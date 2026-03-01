//! Vault scanning and live filesystem watching.

use crate::note::{self, Note};
use notify_debouncer_mini::{DebouncedEventKind, new_debouncer};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{info, warn};
use walkdir::WalkDir;

/// Snapshot of a vault's state.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct VaultState {
    /// All notes indexed by their relative path.
    pub notes: HashMap<PathBuf, Note>,
}

impl VaultState {
    /// Create an empty vault state.
    pub fn new() -> Self {
        Self {
            notes: HashMap::new(),
        }
    }
}

impl Default for VaultState {
    fn default() -> Self {
        Self::new()
    }
}

/// Scan the vault directory and build the initial VaultState.
pub fn scan_vault(vault_root: &Path) -> VaultState {
    let mut notes = HashMap::new();

    for entry in WalkDir::new(vault_root)
        .into_iter()
        .filter_entry(|e| !is_hidden(e))
        .filter_map(|e| e.ok())
    {
        let path = entry.path();

        // Only process .md files
        if path.extension().and_then(|e| e.to_str()) != Some("md") {
            continue;
        }

        let rel_path = match path.strip_prefix(vault_root) {
            Ok(p) => p.to_path_buf(),
            Err(_) => continue,
        };

        match note::parse_note(vault_root, &rel_path) {
            Ok(note) => {
                notes.insert(rel_path, note);
            }
            Err(e) => {
                warn!("Failed to parse {}: {}", path.display(), e);
            }
        }
    }

    info!("Vault scanned: {} notes", notes.len());
    VaultState { notes }
}

/// Check if a directory entry is hidden (starts with dot).
fn is_hidden(entry: &walkdir::DirEntry) -> bool {
    entry
        .file_name()
        .to_str()
        .map(|s| s.starts_with('.'))
        .unwrap_or(false)
}

/// Watches a vault directory for filesystem changes and updates VaultState.
pub struct VaultWatcher {
    vault_root: PathBuf,
    state: Arc<RwLock<VaultState>>,
}

impl VaultWatcher {
    /// Create a new watcher for the given vault.
    pub fn new(vault_root: PathBuf, state: Arc<RwLock<VaultState>>) -> Self {
        Self { vault_root, state }
    }

    /// Start watching for filesystem changes. Runs until cancelled.
    pub async fn watch(&self) -> notify::Result<()> {
        let (tx, rx) = std::sync::mpsc::channel();
        let mut debouncer = new_debouncer(Duration::from_millis(500), tx)?;

        debouncer
            .watcher()
            .watch(&self.vault_root, notify::RecursiveMode::Recursive)?;

        info!("Watching vault: {}", self.vault_root.display());

        // Process events in a blocking thread since the debouncer uses std channels
        let vault_root = self.vault_root.clone();
        let state = self.state.clone();

        tokio::task::spawn_blocking(move || {
            while let Ok(events_result) = rx.recv() {
                match events_result {
                    Ok(events) => {
                        let mut paths_changed = Vec::new();

                        for event in &events {
                            if event.kind != DebouncedEventKind::Any {
                                continue;
                            }

                            let path = &event.path;

                            // Skip non-markdown and hidden files
                            if path.extension().and_then(|e| e.to_str()) != Some("md") {
                                continue;
                            }
                            if path
                                .components()
                                .any(|c| c.as_os_str().to_string_lossy().starts_with('.'))
                            {
                                continue;
                            }

                            if let Ok(rel_path) = path.strip_prefix(&vault_root) {
                                paths_changed.push(rel_path.to_path_buf());
                            }
                        }

                        if !paths_changed.is_empty() {
                            let vault_root = vault_root.clone();
                            let state = state.clone();

                            // Use a blocking write since we're already on a blocking thread
                            let rt = tokio::runtime::Handle::current();
                            rt.block_on(async {
                                let mut vault = state.write().await;
                                for rel_path in &paths_changed {
                                    let abs_path = vault_root.join(rel_path);
                                    if abs_path.exists() {
                                        match note::parse_note(&vault_root, rel_path) {
                                            Ok(note) => {
                                                info!("Updated: {}", rel_path.display());
                                                vault.notes.insert(rel_path.clone(), note);
                                            }
                                            Err(e) => {
                                                warn!(
                                                    "Failed to parse {}: {}",
                                                    rel_path.display(),
                                                    e
                                                );
                                            }
                                        }
                                    } else {
                                        info!("Deleted: {}", rel_path.display());
                                        vault.notes.remove(rel_path);
                                    }
                                }
                            });
                        }
                    }
                    Err(e) => {
                        warn!("Watch error: {:?}", e);
                    }
                }
            }
        })
        .await
        .map_err(|e| notify::Error::generic(&format!("Watcher task failed: {}", e)))?;

        Ok(())
    }
}
