//! # ob — Obsidian Vault Library
//!
//! Pure Rust library for parsing and working with Obsidian vaults.
//! Provides markdown parsing, task extraction, wikilink resolution,
//! link graphs, and filesystem watching for live reload.

pub mod daily_notes;
pub mod html;
pub mod link;
pub mod link_graph;
pub mod markdown;
pub mod note;
pub mod task;
pub mod vault;
