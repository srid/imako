//! Simplified, serializable Markdown AST types.
//!
//! These are converted from comrak's internal AST nodes into a clean
//! enum-based representation that can be serialized to JSON and sent
//! to the Dioxus client for rendering.

use serde::{Deserialize, Serialize};

/// A Markdown document — a sequence of block elements.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Document {
    pub blocks: Vec<Block>,
}

/// Block-level Markdown elements.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "t", content = "c")]
pub enum Block {
    /// Paragraph containing inline elements.
    Paragraph(Vec<Inline>),

    /// Heading with level (1-6) and inline content.
    Heading { level: u8, content: Vec<Inline> },

    /// Unordered list of items (each item is a list of blocks).
    BulletList(Vec<Vec<Block>>),

    /// Ordered list with start number and items.
    OrderedList {
        start: usize,
        items: Vec<Vec<Block>>,
    },

    /// Fenced or indented code block with optional language.
    CodeBlock {
        language: Option<String>,
        code: String,
    },

    /// Block quote containing blocks.
    BlockQuote(Vec<Block>),

    /// Horizontal rule / thematic break.
    ThematicBreak,

    /// An HTML block (raw HTML).
    HtmlBlock(String),

    /// A task list item (checkbox + content).
    /// This is extracted from list items during AST conversion.
    TaskListItem { checked: bool, content: Vec<Block> },
}

/// Inline-level Markdown elements.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "t", content = "c")]
pub enum Inline {
    /// Plain text.
    Text(String),

    /// Soft line break (rendered as space).
    SoftBreak,

    /// Hard line break.
    LineBreak,

    /// Inline code span.
    Code(String),

    /// Emphasized (italic) content.
    Emph(Vec<Inline>),

    /// Strong (bold) content.
    Strong(Vec<Inline>),

    /// Strikethrough content.
    Strikethrough(Vec<Inline>),

    /// A hyperlink.
    Link {
        url: String,
        title: String,
        content: Vec<Inline>,
    },

    /// An image.
    Image {
        url: String,
        title: String,
        alt: String,
    },

    /// Raw inline HTML.
    HtmlInline(String),
}

/// Convert a comrak AST into our simplified Document.
pub fn comrak_to_document<'a>(root: &'a comrak::nodes::AstNode<'a>) -> Document {
    let blocks = convert_children_to_blocks(root);
    Document { blocks }
}

fn convert_children_to_blocks<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Vec<Block> {
    node.children().filter_map(convert_node_to_block).collect()
}

fn convert_node_to_block<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Option<Block> {
    use comrak::nodes::NodeValue;

    let val = &node.data.borrow().value;
    match val {
        NodeValue::Paragraph => {
            let inlines = convert_children_to_inlines(node);
            Some(Block::Paragraph(inlines))
        }
        NodeValue::Heading(heading) => {
            let inlines = convert_children_to_inlines(node);
            Some(Block::Heading {
                level: heading.level,
                content: inlines,
            })
        }
        NodeValue::List(list) => {
            let items: Vec<Vec<Block>> = node
                .children()
                .map(|item| convert_children_to_blocks(item))
                .collect();

            match list.list_type {
                comrak::nodes::ListType::Bullet => Some(Block::BulletList(items)),
                comrak::nodes::ListType::Ordered => Some(Block::OrderedList {
                    start: list.start,
                    items,
                }),
            }
        }
        NodeValue::Item(_) => {
            // Items are handled by List — shouldn't appear at top level
            None
        }
        NodeValue::CodeBlock(code_block) => {
            let language = if code_block.info.is_empty() {
                None
            } else {
                // The info string may contain more than just the language
                Some(
                    code_block
                        .info
                        .split_whitespace()
                        .next()
                        .unwrap_or("")
                        .to_string(),
                )
            };
            Some(Block::CodeBlock {
                language,
                code: code_block.literal.clone(),
            })
        }
        NodeValue::BlockQuote => {
            let children = convert_children_to_blocks(node);
            Some(Block::BlockQuote(children))
        }
        NodeValue::ThematicBreak => Some(Block::ThematicBreak),
        NodeValue::HtmlBlock(html) => Some(Block::HtmlBlock(html.literal.clone())),
        NodeValue::TaskItem(checked) => {
            let content = convert_children_to_blocks(node);
            Some(Block::TaskListItem {
                checked: checked.is_some(),
                content,
            })
        }
        // For now, skip nodes we don't handle yet (tables, footnotes, etc.)
        // They'll be added in later phases.
        _ => {
            // Try to render children as blocks as a fallback
            let children = convert_children_to_blocks(node);
            if children.is_empty() {
                None
            } else {
                // Wrap in a generic container
                Some(Block::BlockQuote(children))
            }
        }
    }
}

fn convert_children_to_inlines<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Vec<Inline> {
    node.children()
        .flat_map(|child| convert_node_to_inlines(child))
        .collect()
}

fn convert_node_to_inlines<'a>(node: &'a comrak::nodes::AstNode<'a>) -> Vec<Inline> {
    use comrak::nodes::NodeValue;

    let val = &node.data.borrow().value;
    match val {
        NodeValue::Text(text) => vec![Inline::Text(text.clone())],
        NodeValue::SoftBreak => vec![Inline::SoftBreak],
        NodeValue::LineBreak => vec![Inline::LineBreak],
        NodeValue::Code(code) => vec![Inline::Code(code.literal.clone())],
        NodeValue::Emph => {
            let children = convert_children_to_inlines(node);
            vec![Inline::Emph(children)]
        }
        NodeValue::Strong => {
            let children = convert_children_to_inlines(node);
            vec![Inline::Strong(children)]
        }
        NodeValue::Strikethrough => {
            let children = convert_children_to_inlines(node);
            vec![Inline::Strikethrough(children)]
        }
        NodeValue::Link(link) => {
            let children = convert_children_to_inlines(node);
            vec![Inline::Link {
                url: link.url.clone(),
                title: link.title.clone(),
                content: children,
            }]
        }
        NodeValue::Image(link) => {
            let alt = extract_text_from_children(node);
            vec![Inline::Image {
                url: link.url.clone(),
                title: link.title.clone(),
                alt,
            }]
        }
        NodeValue::HtmlInline(html) => vec![Inline::HtmlInline(html.clone())],
        // For inline elements we don't handle yet, try to extract text
        _ => {
            let children = convert_children_to_inlines(node);
            if children.is_empty() {
                vec![]
            } else {
                children
            }
        }
    }
}

/// Extract plain text from all children (for alt text, etc.)
fn extract_text_from_children<'a>(node: &'a comrak::nodes::AstNode<'a>) -> String {
    let mut text = String::new();
    for child in node.children() {
        extract_text_recursive(child, &mut text);
    }
    text
}

fn extract_text_recursive<'a>(node: &'a comrak::nodes::AstNode<'a>, out: &mut String) {
    use comrak::nodes::NodeValue;
    match &node.data.borrow().value {
        NodeValue::Text(t) => out.push_str(t),
        NodeValue::Code(c) => out.push_str(&c.literal),
        NodeValue::SoftBreak | NodeValue::LineBreak => out.push(' '),
        _ => {
            for child in node.children() {
                extract_text_recursive(child, out);
            }
        }
    }
}
