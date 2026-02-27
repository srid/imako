//! CLI argument parsing.

use clap::Parser;
use std::path::PathBuf;

/// Imako — journaling and planning for your Obsidian notebook.
#[derive(Parser, Debug)]
#[command(name = "imako", version, about)]
pub struct Cli {
    /// Path to the Obsidian vault directory
    pub vault_path: PathBuf,

    /// Port to serve the web application on
    #[arg(long, default_value = "4009")]
    pub port: u16,

    /// Host to bind to
    #[arg(long, default_value = "localhost")]
    pub host: String,
}
