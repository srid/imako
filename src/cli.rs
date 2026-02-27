//! CLI argument parsing.

use clap::Parser;
use std::path::PathBuf;

/// Imako — journaling and planning for your Obsidian notebook.
#[derive(Parser, Debug)]
#[command(name = "imako", version, about)]
pub struct Cli {
    /// Path to your Obsidian vault
    pub vault_path: Option<PathBuf>,

    /// Port to run the web server on
    #[arg(long, default_value = "8080")]
    pub port: u16,

    /// Host to bind the web server to
    #[arg(long, default_value = "127.0.0.1")]
    pub host: String,
}
