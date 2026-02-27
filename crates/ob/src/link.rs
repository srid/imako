//! Wikilink types, resolution, and Obsidian URL builder.

use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};

use crate::note::Note;

/// A wikilink reference to another note.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct WikiLink {
    /// The target slug (e.g. "Page" or "Folder/Page")
    pub target: String,
    /// Optional display alias
    pub alias: Option<String>,
}

impl WikiLink {
    /// Parse a wikilink from `[[target]]` or `[[target|alias]]` syntax.
    pub fn parse(s: &str) -> Option<Self> {
        let inner = s.strip_prefix("[[")?.strip_suffix("]]")?;
        if inner.is_empty() {
            return None;
        }
        let (target, alias) = match inner.split_once('|') {
            Some((t, a)) => (t.to_string(), Some(a.to_string())),
            None => (inner.to_string(), None),
        };
        Some(WikiLink { target, alias })
    }
}

/// Resolve a wikilink target against a set of notes.
///
/// Matches by suffix: `[[Page]]` matches "Folder/Page.md", "Other/Page.md", etc.
/// Returns the first match.
pub fn resolve_wikilink(target: &str, notes: &[Note]) -> Option<String> {
    // Try exact match first (without .md extension)
    for note in notes {
        let stem = note.path.trim_end_matches(".md");
        if stem == target {
            return Some(note.path.clone());
        }
    }
    // Try suffix match
    let suffix = format!("/{}", target);
    for note in notes {
        let stem = note.path.trim_end_matches(".md");
        if stem.ends_with(&suffix) {
            return Some(note.path.clone());
        }
    }
    None
}

/// Build an `obsidian://open` URL for a note.
pub fn obsidian_open_url(vault_name: &str, note_path: &str) -> String {
    let encoded_vault = utf8_percent_encode(vault_name, NON_ALPHANUMERIC).to_string();
    let encoded_file = utf8_percent_encode(note_path, NON_ALPHANUMERIC).to_string();
    format!(
        "obsidian://open?vault={}&file={}",
        encoded_vault, encoded_file
    )
}

/// URL-encode a path component for internal links.
pub fn encode_path_component(path: &str) -> String {
    utf8_percent_encode(path, NON_ALPHANUMERIC).to_string()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_simple_wikilink() {
        let wl = WikiLink::parse("[[Page]]").unwrap();
        assert_eq!(wl.target, "Page");
        assert_eq!(wl.alias, None);
    }

    #[test]
    fn test_parse_aliased_wikilink() {
        let wl = WikiLink::parse("[[Page|Display Name]]").unwrap();
        assert_eq!(wl.target, "Page");
        assert_eq!(wl.alias, Some("Display Name".to_string()));
    }

    #[test]
    fn test_resolve_wikilink() {
        let notes = vec![
            Note::from_string("Projects/Frontend.md", "# Frontend"),
            Note::from_string("Notes/Ideas.md", "# Ideas"),
        ];
        assert_eq!(
            resolve_wikilink("Frontend", &notes),
            Some("Projects/Frontend.md".to_string())
        );
        assert_eq!(
            resolve_wikilink("Projects/Frontend", &notes),
            Some("Projects/Frontend.md".to_string())
        );
        assert_eq!(resolve_wikilink("NonExistent", &notes), None);
    }
}
