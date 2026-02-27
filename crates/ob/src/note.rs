//! Note parsing and data types for Obsidian notebooks.

use std::path::Path;
use std::time::SystemTime;

use chrono::{DateTime, Utc};
use comrak::Arena;
use serde::{Deserialize, Serialize};

use crate::markdown::parse_markdown;
use crate::task::{self, Task};

/// An Obsidian note parsed from a `.md` file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Note {
    /// Relative path within the vault (e.g. "folder/note.md")
    pub path: String,
    /// YAML frontmatter properties
    pub frontmatter: Option<serde_json::Value>,
    /// Raw markdown content
    pub content: String,
    /// Tasks extracted from this note
    pub tasks: Vec<Task>,
    /// Last modification time
    pub modified_at: DateTime<Utc>,
}

impl Note {
    /// Parse a note from disk.
    ///
    /// `base_path` is the vault root directory.
    /// `rel_path` is the relative path within the vault (used as identity).
    pub fn from_file(base_path: &Path, rel_path: &str) -> Result<Self, NoteError> {
        let abs_path = base_path.join(rel_path);
        let content = std::fs::read_to_string(&abs_path)
            .map_err(|e| NoteError::ReadError(abs_path.display().to_string(), e))?;

        let mtime = std::fs::metadata(&abs_path)
            .and_then(|m| m.modified())
            .unwrap_or(SystemTime::UNIX_EPOCH);
        let modified_at: DateTime<Utc> = mtime.into();

        let arena = Arena::new();
        let parsed = parse_markdown(&arena, &content);

        let tasks = task::extract_tasks(rel_path, parsed.root);

        Ok(Note {
            path: rel_path.to_string(),
            frontmatter: parsed.frontmatter,
            content,
            tasks,
            modified_at,
        })
    }

    /// Parse a note from a string (for testing).
    pub fn from_string(rel_path: &str, content: &str) -> Self {
        let arena = Arena::new();
        let parsed = parse_markdown(&arena, content);
        let tasks = task::extract_tasks(rel_path, parsed.root);

        Note {
            path: rel_path.to_string(),
            frontmatter: parsed.frontmatter,
            content: content.to_string(),
            tasks,
            modified_at: Utc::now(),
        }
    }

    /// All possible wikilink slugs that refer to this note.
    ///
    /// For "Foo/Bar/Qux.md" produces: ["Qux", "Bar/Qux", "Foo/Bar/Qux"]
    pub fn self_refs(&self) -> Vec<String> {
        let stem = self.path.trim_end_matches(".md");
        let parts: Vec<&str> = stem.split('/').collect();
        let mut refs = Vec::new();

        for i in (0..parts.len()).rev() {
            refs.push(parts[i..].join("/"));
        }

        refs
    }
}

/// Errors that can occur when loading a note.
#[derive(Debug, thiserror::Error)]
pub enum NoteError {
    #[error("Failed to read {0}: {1}")]
    ReadError(String, std::io::Error),
    #[error("Failed to parse {0}: {1}")]
    ParseError(String, String),
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_self_refs() {
        let note = Note::from_string("Projects/Team/Frontend.md", "# Frontend");
        let refs = note.self_refs();
        assert_eq!(
            refs,
            vec!["Frontend", "Team/Frontend", "Projects/Team/Frontend"]
        );
    }

    #[test]
    fn test_note_from_string() {
        let note = Note::from_string(
            "test.md",
            "---\ntitle: Test\n---\n\n# Hello\n\n- [ ] Task 1\n- [x] Task 2\n",
        );
        assert_eq!(note.path, "test.md");
        assert!(note.frontmatter.is_some());
        assert_eq!(note.tasks.len(), 2);
    }
}
