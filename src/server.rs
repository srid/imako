//! Server functions for the Dioxus fullstack app.

use dioxus::prelude::*;

/// Get the vault tree data (folder tree + metadata).
#[server]
pub async fn get_vault_tree() -> Result<imako_core::protocol::VaultTreeData, ServerFnError> {
    use imako_core::folder_tree;
    use ob::vault::Vault;

    let vault_path = get_vault_path()?;
    let vault = Vault::load(&vault_path)
        .map_err(|e| ServerFnError::new(format!("Failed to load vault: {}", e)))?;

    let mut tree = folder_tree::build_folder_tree(&vault.notes);
    folder_tree::flatten_tree(&mut tree);

    let daily_notes = vault.daily_notes();
    if let Some(ref config) = vault.daily_notes_config {
        folder_tree::annotate_daily_notes(&mut tree, &daily_notes, config);
    }

    let today = chrono::Local::now().date_naive();

    let info = imako_core::protocol::VaultInfo {
        vault_name: vault_path
            .file_name()
            .map(|s| s.to_string_lossy().to_string())
            .unwrap_or_else(|| "Vault".to_string()),
        today,
        daily_notes_folder: vault.daily_notes_config.as_ref().map(|c| c.folder.clone()),
        today_note_path: vault
            .daily_notes_config
            .as_ref()
            .map(|c| ob::daily_notes::get_today_note_path(c, today)),
    };

    Ok(imako_core::protocol::VaultTreeData {
        info,
        tree,
        daily_notes,
    })
}

/// Get a single note's rendered content + backlinks.
#[server]
pub async fn get_note(path: String) -> Result<imako_core::protocol::NoteData, ServerFnError> {
    use ob::vault::Vault;

    let vault_path = get_vault_path()?;
    let vault = Vault::load(&vault_path)
        .map_err(|e| ServerFnError::new(format!("Failed to load vault: {}", e)))?;

    let note = vault
        .note_by_path(&path)
        .ok_or_else(|| ServerFnError::new(format!("Note not found: {}", path)))?;

    let rendered_html = ob::html::render_note_html(&note.content, &vault.notes);
    let backlinks = vault.backlinks_of(&path);

    Ok(imako_core::protocol::NoteData {
        path: note.path.clone(),
        html: rendered_html,
        backlinks,
        tasks: note.tasks.clone(),
    })
}

/// Get the vault path from server context or environment.
#[cfg(feature = "server")]
fn get_vault_path() -> Result<std::path::PathBuf, ServerFnError> {
    if let Ok(path) = std::env::var("IMAKO_VAULT_PATH") {
        Ok(std::path::PathBuf::from(path))
    } else {
        Err(ServerFnError::new(
            "IMAKO_VAULT_PATH not set. Run: IMAKO_VAULT_PATH=/path/to/vault dx serve",
        ))
    }
}
