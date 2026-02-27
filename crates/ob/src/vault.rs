//! Vault loading and live-reload.
//!
//! Scans vault directory for `.md` files, parses all notes,
//! builds link graph, and provides filesystem watching for live updates.

use std::path::{Path, PathBuf};

use tokio::sync::watch;
use tracing::{info, warn};
use walkdir::WalkDir;

use crate::daily_notes::{self, DailyNote, DailyNotesConfig};
use crate::link_graph::LinkGraph;
use crate::note::Note;
use crate::task::Task;

/// An Obsidian vault with all parsed notes, tasks, and link graph.
#[derive(Debug, Clone)]
pub struct Vault {
    /// All notes in the vault
    pub notes: Vec<Note>,
    /// All tasks across all notes
    pub tasks: Vec<Task>,
    /// Directed graph of wikilink references
    pub link_graph: LinkGraph,
    /// Daily notes plugin configuration (if enabled)
    pub daily_notes_config: Option<DailyNotesConfig>,
}

impl Vault {
    /// Load a vault from disk (one-shot, no watching).
    pub fn load(vault_path: &Path) -> Result<Self, VaultError> {
        let daily_notes_config = daily_notes::load_daily_notes_config(vault_path);

        let notes = scan_notes(vault_path)?;
        let tasks: Vec<Task> = notes.iter().flat_map(|n| n.tasks.clone()).collect();
        let link_graph = LinkGraph::build(&notes);

        info!("Vault loaded: {} notes, {} tasks", notes.len(), tasks.len());

        Ok(Vault {
            notes,
            tasks,
            link_graph,
            daily_notes_config,
        })
    }

    /// Get all daily notes, sorted by date (most recent first).
    pub fn daily_notes(&self) -> Vec<DailyNote> {
        let Some(ref config) = self.daily_notes_config else {
            return Vec::new();
        };

        let mut daily: Vec<DailyNote> = self
            .notes
            .iter()
            .filter_map(|note| daily_notes::mk_daily_note(config, &note.path))
            .collect();
        daily.sort_by(|a, b| b.day.cmp(&a.day));
        daily
    }

    /// Find a note by its vault-relative path.
    pub fn note_by_path(&self, path: &str) -> Option<&Note> {
        self.notes.iter().find(|n| n.path == path)
    }

    /// Get backlinks for a note path.
    pub fn backlinks_of(&self, path: &str) -> Vec<String> {
        self.link_graph.backlinks_of(path)
    }
}

/// Start watching a vault for filesystem changes.
///
/// Returns a watch receiver that emits new `Vault` snapshots on file changes.
pub async fn watch_vault(vault_path: PathBuf) -> Result<watch::Receiver<Vault>, VaultError> {
    let initial = Vault::load(&vault_path)?;
    let (tx, rx) = watch::channel(initial);

    let path = vault_path.clone();
    tokio::spawn(async move {
        use notify::{Event, RecursiveMode, Watcher};

        let (notify_tx, mut notify_rx) = tokio::sync::mpsc::channel::<Event>(100);

        let mut watcher = notify::recommended_watcher(move |res: Result<Event, _>| {
            if let Ok(event) = res {
                let _ = notify_tx.blocking_send(event);
            }
        })
        .expect("Failed to create filesystem watcher");

        watcher
            .watch(&path, RecursiveMode::Recursive)
            .expect("Failed to watch vault directory");

        info!("Watching vault for changes: {}", path.display());

        // Debounce: wait a bit after events before reloading
        loop {
            // Wait for first event
            if notify_rx.recv().await.is_none() {
                break;
            }

            // Drain additional events (debounce)
            tokio::time::sleep(tokio::time::Duration::from_millis(200)).await;
            while notify_rx.try_recv().is_ok() {}

            // Reload vault
            match Vault::load(&path) {
                Ok(new_vault) => {
                    info!(
                        "Vault reloaded: {} notes, {} tasks",
                        new_vault.notes.len(),
                        new_vault.tasks.len()
                    );
                    let _ = tx.send(new_vault);
                }
                Err(e) => {
                    warn!("Failed to reload vault: {}", e);
                }
            }
        }
    });

    Ok(rx)
}

/// Scan a directory for markdown files and parse them as notes.
fn scan_notes(vault_path: &Path) -> Result<Vec<Note>, VaultError> {
    let mut notes = Vec::new();

    for entry in WalkDir::new(vault_path)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
    {
        let path = entry.path();

        // Skip hidden directories (e.g. .obsidian, .git)
        if path
            .components()
            .any(|c| c.as_os_str().to_string_lossy().starts_with('.'))
        {
            // Skip, but allow the root itself
            if path != vault_path {
                continue;
            }
        }

        // Only process .md files
        if path.extension().is_none_or(|ext| ext != "md") {
            continue;
        }

        let rel_path = path
            .strip_prefix(vault_path)
            .map_err(|_| VaultError::PathError(path.display().to_string()))?
            .to_string_lossy()
            .to_string();

        match Note::from_file(vault_path, &rel_path) {
            Ok(note) => notes.push(note),
            Err(e) => {
                warn!("Skipping {}: {}", rel_path, e);
            }
        }
    }

    Ok(notes)
}

/// Errors that can occur when loading a vault.
#[derive(Debug, thiserror::Error)]
pub enum VaultError {
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),
    #[error("Path error: {0}")]
    PathError(String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_load_example_vault() {
        let vault_path = Path::new(env!("CARGO_MANIFEST_DIR"))
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("example");

        if !vault_path.exists() {
            eprintln!("Skipping test: example vault not found at {:?}", vault_path);
            return;
        }

        let vault = Vault::load(&vault_path).expect("Failed to load vault");
        assert!(!vault.notes.is_empty(), "Vault should have notes");
        assert!(!vault.tasks.is_empty(), "Vault should have tasks");
    }
}
