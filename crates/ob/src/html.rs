//! Markdown to HTML rendering with resolved wikilinks and callouts.
//!
//! Renders comrak AST to HTML with:
//! - Wikilinks resolved to internal routes (`/p/<path>`)
//! - Callout blockquotes transformed into styled divs

use comrak::nodes::{AstNode, NodeValue};
use comrak::Arena;

use crate::link;
use crate::markdown::{comrak_options, render_html};
use crate::note::Note;

/// Render a note's markdown content to HTML with resolved wikilinks.
///
/// Wikilinks are resolved against the provided notes index:
/// - Resolved: `<a href="/p/<path>" data-wikilink="true">display</a>`
/// - Broken: `<a data-wikilink="true" data-broken="true">display</a>`
pub fn render_note_html(content: &str, notes: &[Note]) -> String {
    // For now, use comrak's built-in HTML rendering.
    // Wikilinks are rendered by comrak with its wikilink extension.
    // We post-process the HTML to resolve link targets.

    let arena = Arena::new();
    let options = comrak_options();
    let root = comrak::parse_document(&arena, content, &options);

    // Walk the AST and resolve wikilinks
    resolve_wikilinks_in_ast(root, notes);

    // Render to HTML
    render_html(root)
}

/// Walk the AST and resolve wikilink URLs.
fn resolve_wikilinks_in_ast<'a>(node: &'a AstNode<'a>, notes: &[Note]) {
    match &mut node.data.borrow_mut().value {
        NodeValue::WikiLink(ref mut wl) => {
            // comrak stores wikilink with url field
            let target = &wl.url;
            if let Some(resolved_path) = link::resolve_wikilink(target, notes) {
                let encoded = link::encode_path_component(&resolved_path);
                wl.url = format!("/p/{}", encoded);
            }
        }
        NodeValue::Link(ref mut _nl) => {
            // Check if this is a wikilink-style link
            // (comrak may represent them as regular links)
        }
        _ => {}
    }

    for child in node.children() {
        resolve_wikilinks_in_ast(child, notes);
    }
}

/// Transform Obsidian callout blockquotes into styled HTML.
///
/// Input: `> [!NOTE]\n> This is a note`
/// Output: `<div class="callout callout-note"><p>This is a note</p></div>`
pub fn transform_callouts(html: &str) -> String {
    // Simple post-processing: find blockquote patterns with callout markers
    let mut result = String::with_capacity(html.len());
    let lines: Vec<&str> = html.lines().collect();
    let mut i = 0;

    while i < lines.len() {
        let line = lines[i];

        // Detect callout start: <blockquote> followed by [!TYPE]
        if line.trim() == "<blockquote>" && i + 1 < lines.len() {
            let next_line = lines[i + 1].trim();
            if let Some(callout_type) = parse_callout_marker(next_line) {
                // Found a callout — transform it
                result.push_str(&format!(
                    "<div class=\"callout callout-{}\">\n",
                    callout_type.to_lowercase()
                ));
                result.push_str(&format!(
                    "<div class=\"callout-title\">{}</div>\n",
                    callout_type
                ));
                i += 2; // Skip <blockquote> and marker line

                // Collect callout content until </blockquote>
                while i < lines.len() && lines[i].trim() != "</blockquote>" {
                    result.push_str(lines[i]);
                    result.push('\n');
                    i += 1;
                }
                result.push_str("</div>\n");
                i += 1; // Skip </blockquote>
                continue;
            }
        }

        result.push_str(line);
        result.push('\n');
        i += 1;
    }

    result
}

/// Parse a callout marker like `<p>[!NOTE]` or `<p>[!WARNING]`.
fn parse_callout_marker(line: &str) -> Option<String> {
    let content = line.strip_prefix("<p>")?.strip_prefix("[!")?;
    let end = content.find(']')?;
    let callout_type = &content[..end];
    if callout_type.is_empty() {
        return None;
    }
    Some(callout_type.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_render_basic_html() {
        let html = render_note_html("# Hello\n\nWorld", &[]);
        assert!(html.contains("<h1>"));
        assert!(html.contains("Hello"));
        assert!(html.contains("World"));
    }

    #[test]
    fn test_callout_parsing() {
        let marker = parse_callout_marker("<p>[!NOTE]");
        assert_eq!(marker, Some("NOTE".to_string()));

        let marker = parse_callout_marker("<p>[!WARNING] Important");
        assert_eq!(marker, Some("WARNING".to_string()));
    }
}
