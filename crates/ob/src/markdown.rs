//! Markdown parsing using comrak with Obsidian extensions.
//!
//! Parses `.md` files with:
//! - CommonMark + GFM extensions (tables, strikethrough, task lists, footnotes)
//! - Wikilink syntax (`[[Page]]`, `[[Page|alias]]`)
//! - YAML frontmatter extraction
//! - Obsidian callout blocks (`> [!NOTE]`)

use comrak::nodes::{AstNode, NodeValue};
use comrak::{parse_document, Arena, ExtensionOptions, Options, ParseOptions, RenderOptions};

/// Comrak options configured for Obsidian-compatible markdown.
pub fn comrak_options() -> Options<'static> {
    Options {
        extension: ExtensionOptions {
            strikethrough: true,
            table: true,
            autolink: true,
            tasklist: true,
            footnotes: true,
            description_lists: true,
            front_matter_delimiter: Some("---".to_string()),
            wikilinks_title_after_pipe: true,
            ..Default::default()
        },
        parse: ParseOptions {
            smart: false,
            ..Default::default()
        },
        render: RenderOptions {
            unsafe_: true,
            ..Default::default()
        },
    }
}

/// Parsed markdown result containing frontmatter and AST.
pub struct ParsedMarkdown<'a> {
    pub frontmatter: Option<serde_json::Value>,
    pub root: &'a AstNode<'a>,
}

/// Parse a markdown string into a comrak AST with extracted frontmatter.
///
/// The `arena` parameter is the comrak arena allocator that owns the AST nodes.
pub fn parse_markdown<'a>(arena: &'a Arena<AstNode<'a>>, source: &str) -> ParsedMarkdown<'a> {
    let options = comrak_options();
    let root = parse_document(arena, source, &options);

    // Extract frontmatter from the AST
    let frontmatter = extract_frontmatter(root);

    ParsedMarkdown { frontmatter, root }
}

/// Extract YAML frontmatter from the AST root.
fn extract_frontmatter<'a>(root: &'a AstNode<'a>) -> Option<serde_json::Value> {
    for child in root.children() {
        if let NodeValue::FrontMatter(ref raw) = child.data.borrow().value {
            // comrak includes the delimiter lines, e.g. "---\ntitle: Test\n---\n"
            // Strip the opening/closing --- lines
            let lines: Vec<&str> = raw.lines().collect();
            let yaml_lines: Vec<&str> = lines
                .iter()
                .filter(|line| line.trim() != "---")
                .copied()
                .collect();
            let yaml_str = yaml_lines.join("\n");
            if yaml_str.trim().is_empty() {
                return None;
            }
            if let Ok(value) = serde_yaml::from_str::<serde_json::Value>(&yaml_str) {
                return Some(value);
            }
        }
    }
    None
}

/// Render a comrak AST to HTML string.
pub fn render_html<'a>(root: &'a AstNode<'a>) -> String {
    let options = comrak_options();
    let mut output = Vec::new();
    comrak::format_html(root, &options, &mut output).unwrap();
    String::from_utf8(output).unwrap()
}

/// Extract plain text from a comrak AST node (for task descriptions, etc.)
pub fn extract_text<'a>(node: &'a AstNode<'a>) -> String {
    let mut text = String::new();
    collect_text(node, &mut text);
    text
}

fn collect_text<'a>(node: &'a AstNode<'a>, buf: &mut String) {
    match &node.data.borrow().value {
        NodeValue::Text(t) => buf.push_str(t),
        NodeValue::SoftBreak | NodeValue::LineBreak => buf.push(' '),
        NodeValue::Code(c) => buf.push_str(&c.literal),
        _ => {
            for child in node.children() {
                collect_text(child, buf);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_basic_markdown() {
        let arena = Arena::new();
        let result = parse_markdown(&arena, "# Hello\n\nWorld");
        assert!(result.frontmatter.is_none());
        let html = render_html(result.root);
        assert!(html.contains("<h1>"));
        assert!(html.contains("Hello"));
    }

    #[test]
    fn test_parse_frontmatter() {
        let arena = Arena::new();
        let result = parse_markdown(
            &arena,
            "---\ntitle: Test\ntags:\n  - foo\n  - bar\n---\n\n# Content",
        );
        let fm = result.frontmatter.unwrap();
        assert_eq!(fm["title"], "Test");
        assert_eq!(fm["tags"][0], "foo");
    }

    #[test]
    fn test_wikilinks() {
        let arena = Arena::new();
        let result = parse_markdown(&arena, "Link to [[SomePage]]");
        let html = render_html(result.root);
        assert!(html.contains("SomePage"));
    }
}
