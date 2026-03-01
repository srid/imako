//! Imako — Journaling and planning for Obsidian notebooks.
//!
//! Dioxus fullstack application: a single Rust binary that serves
//! both the server (vault operations) and client (WASM UI).

#![allow(non_snake_case)]

use dioxus::prelude::*;

mod components;
mod pages;

#[cfg(feature = "server")]
mod server;

/// CLI arguments for the Imako server.
#[cfg(feature = "server")]
#[derive(clap::Parser)]
#[command(
    name = "imako",
    about = "Journaling and planning for Obsidian notebooks"
)]
struct Cli {
    /// Path to the Obsidian vault directory
    #[arg(long, env = "VAULT_PATH", default_value = "example")]
    vault: String,
}

/// Shared types used by both server and client.
pub mod shared {
    use ob::{FolderNode, Note};
    use serde::{Deserialize, Serialize};

    /// Basic vault metadata.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct VaultInfo {
        pub vault_name: String,
        pub vault_path: String,
        pub today: String,
    }

    /// Response for a folder tree request.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct FolderTreeData {
        pub info: VaultInfo,
        pub tree: FolderNode,
    }

    /// Response for a note request.
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct NoteData {
        pub info: VaultInfo,
        pub note: Note,
    }
}

// --- Server Functions ---

/// Fetch the vault info and folder tree.
#[server]
pub async fn get_folder_tree() -> Result<shared::FolderTreeData, ServerFnError> {
    server::api::get_folder_tree_impl().await
}

/// Fetch a specific note by its vault-relative path.
#[server]
pub async fn get_note(path: String) -> Result<shared::NoteData, ServerFnError> {
    server::api::get_note_impl(path).await
}

// --- Routing ---

#[derive(Debug, Clone, Routable, PartialEq)]
enum Route {
    #[route("/")]
    Home {},

    #[route("/p/:..path")]
    VaultPath { path: Vec<String> },
}

fn App() -> Element {
    rsx! {
        Router::<Route> {}
    }
}

#[component]
fn Home() -> Element {
    rsx! {
        pages::vault_page::VaultPage { path: None }
    }
}

#[component]
fn VaultPath(path: Vec<String>) -> Element {
    let joined = path.join("/");
    let p = if joined.is_empty() {
        None
    } else {
        Some(joined)
    };
    rsx! {
        pages::vault_page::VaultPage { path: p }
    }
}

fn main() {
    // Initialize server state before launching Dioxus
    #[cfg(feature = "server")]
    {
        use clap::Parser;
        let cli = Cli::parse();
        let vault_root = std::path::PathBuf::from(&cli.vault)
            .canonicalize()
            .unwrap_or_else(|_| panic!("Vault path not found: {}", cli.vault));
        tracing::info!("Loading vault: {}", vault_root.display());
        server::api::init(vault_root);
    }

    dioxus::launch(App);
}
