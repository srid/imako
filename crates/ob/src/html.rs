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
    let arena = Arena::new();
    let options = comrak_options();
    let root = comrak::parse_document(&arena, content, &options);

    // Collect resolved and broken wikilink targets
    let mut resolved_targets: Vec<String> = Vec::new();
    let mut broken_targets: Vec<String> = Vec::new();
    collect_wikilink_targets(root, notes, &mut resolved_targets, &mut broken_targets);

    // Resolve wikilink URLs in the AST
    resolve_wikilinks_in_ast(root, notes);

    // Render to HTML
    let html = render_html(root);

    // Post-process HTML: add data-wikilink/data-broken attrs and target=_blank
    post_process_html(&html, &resolved_targets, &broken_targets)
}

/// Collect wikilink targets before resolution for post-processing.
fn collect_wikilink_targets<'a>(
    node: &'a AstNode<'a>,
    notes: &[Note],
    resolved: &mut Vec<String>,
    broken: &mut Vec<String>,
) {
    if let NodeValue::WikiLink(ref wl) = node.data.borrow().value {
        let target = &wl.url;
        if link::resolve_wikilink(target, notes).is_some() {
            resolved.push(target.clone());
        } else {
            broken.push(target.clone());
        }
    }
    for child in node.children() {
        collect_wikilink_targets(child, notes, resolved, broken);
    }
}

/// Post-process HTML to add data attributes and target=_blank.
fn post_process_html(html: &str, _resolved: &[String], broken: &[String]) -> String {
    let mut result = html.to_string();

    // Add data-wikilink to internal links (/p/ prefix)
    result = result.replace("<a href=\"/p/", "<a data-wikilink=\"true\" href=\"/p/");

    // Add data-broken to broken wikilinks (rendered by comrak without href)
    for target in broken {
        // comrak renders unresolved wikilinks with the original target as text
        // Find and add data-broken attribute
        let search = format!("<a href=\"{}\"", target);
        let replace = format!(
            "<a data-wikilink=\"true\" data-broken=\"true\" href=\"{}\"",
            target
        );
        result = result.replace(&search, &replace);

        // Also handle wikilinks rendered without href (comrak keeps the wikilink URL)
        let search2 = format!("<a href=\"{}", target);
        if result.contains(&search2) && !result.contains("data-broken") {
            result = result.replace(
                &search2,
                &format!(
                    "<a data-wikilink=\"true\" data-broken=\"true\" href=\"{}",
                    target
                ),
            );
        }
    }

    // Add target=_blank to external links (http/https)
    result = result.replace(
        "<a href=\"http://",
        "<a target=\"_blank\" rel=\"noopener\" href=\"http://",
    );
    result = result.replace(
        "<a href=\"https://",
        "<a target=\"_blank\" rel=\"noopener\" href=\"https://",
    );

    result
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
