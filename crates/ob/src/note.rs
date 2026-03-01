//! Note parsing — reads Obsidian markdown files into structured data.

use crate::markdown::{Document, comrak_to_document};
use comrak::{Arena, Options, parse_document};
use std::path::{Path, PathBuf};

/// A parsed Obsidian note.
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize, PartialEq)]
pub struct Note {
    /// Relative path within the vault (e.g., "Notes/Welcome.md").
    pub path: PathBuf,

    /// Parsed markdown content as a simplified AST.
    pub content: Document,
}

/// Parse a markdown file into a Note.
///
/// `vault_root` is the vault directory.
/// `rel_path` is the path relative to the vault root.
pub fn parse_note(vault_root: &Path, rel_path: &Path) -> std::io::Result<Note> {
    let abs_path = vault_root.join(rel_path);
    let source = std::fs::read_to_string(&abs_path)?;
    let content = parse_markdown(&source);
    Ok(Note {
        path: rel_path.to_path_buf(),
        content,
    })
}

/// Parse a markdown string into a Document AST using comrak.
pub fn parse_markdown(source: &str) -> Document {
    let arena = Arena::new();
    let options = comrak_options();
    let root = parse_document(&arena, source, &options);
    comrak_to_document(root)
}

/// Standard comrak options for Obsidian-compatible parsing.
fn comrak_options() -> Options<'static> {
    let mut options = Options::default();

    // GFM extensions
    options.extension.strikethrough = true;
    options.extension.table = true;
    options.extension.autolink = true;
    options.extension.tasklist = true;

    // Additional extensions for Obsidian compatibility
    options.extension.footnotes = true;
    options.extension.description_lists = true;
    options.extension.superscript = true;
    options.extension.math_dollars = true;
    options.extension.math_code = true;
    options.extension.wikilinks_title_after_pipe = true;
    options.extension.underline = true;
    options.extension.front_matter_delimiter = Some("---".to_string());

    options
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::markdown::{Block, Inline};

    #[test]
    fn parse_simple_paragraph() {
        let doc = parse_markdown("Hello, world!");
        assert_eq!(doc.blocks.len(), 1);
        match &doc.blocks[0] {
            Block::Paragraph(inlines) => {
                assert_eq!(inlines.len(), 1);
                match &inlines[0] {
                    Inline::Text(t) => assert_eq!(t, "Hello, world!"),
                    other => panic!("Expected Text, got {:?}", other),
                }
            }
            other => panic!("Expected Paragraph, got {:?}", other),
        }
    }

    #[test]
    fn parse_heading() {
        let doc = parse_markdown("# Title\n\nSome text.");
        assert_eq!(doc.blocks.len(), 2);
        match &doc.blocks[0] {
            Block::Heading { level, content } => {
                assert_eq!(*level, 1);
                assert!(!content.is_empty());
            }
            other => panic!("Expected Heading, got {:?}", other),
        }
    }

    #[test]
    fn parse_bullet_list() {
        let doc = parse_markdown("- Item 1\n- Item 2\n- Item 3\n");
        assert_eq!(doc.blocks.len(), 1);
        match &doc.blocks[0] {
            Block::BulletList(items) => assert_eq!(items.len(), 3),
            other => panic!("Expected BulletList, got {:?}", other),
        }
    }

    #[test]
    fn parse_code_block() {
        let doc = parse_markdown("```rust\nfn main() {}\n```\n");
        assert_eq!(doc.blocks.len(), 1);
        match &doc.blocks[0] {
            Block::CodeBlock { language, code } => {
                assert_eq!(language.as_deref(), Some("rust"));
                assert_eq!(code.trim(), "fn main() {}");
            }
            other => panic!("Expected CodeBlock, got {:?}", other),
        }
    }

    #[test]
    fn parse_emphasis_and_strong() {
        let doc = parse_markdown("This is *italic* and **bold** text.");
        match &doc.blocks[0] {
            Block::Paragraph(inlines) => {
                // Should contain Text, Emph, Text, Strong, Text
                let has_emph = inlines.iter().any(|i| matches!(i, Inline::Emph(_)));
                let has_strong = inlines.iter().any(|i| matches!(i, Inline::Strong(_)));
                assert!(has_emph, "Missing Emph in: {:?}", inlines);
                assert!(has_strong, "Missing Strong in: {:?}", inlines);
            }
            other => panic!("Expected Paragraph, got {:?}", other),
        }
    }
}
