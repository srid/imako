//! # ob — Obsidian Vault Library
//!
//! Pure Rust library for reading and watching Obsidian vaults.
//! No UI dependencies — can be used from any Rust application.

pub mod folder_tree;
pub mod markdown;
pub mod note;
pub mod vault;

pub use folder_tree::FolderNode;
pub use markdown::{Block, Inline};
pub use note::Note;
pub use vault::{VaultState, VaultWatcher};
