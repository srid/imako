//! Task extraction from Obsidian markdown.
//!
//! Extracts tasks from comrak AST bullet/ordered lists, parsing checkbox markers
//! (`- [ ]`, `- [x]`, `- [/]`, `- [-]`) and obsidian-tasks metadata.
//! Tracks parent-child task hierarchy with DFS numbering.

pub mod properties;
pub mod recurrence;

use chrono::NaiveDate;
use comrak::nodes::{AstNode, NodeValue};
use serde::{Deserialize, Serialize};

use self::properties::parse_task_text;

/// Task status markers from Obsidian.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum TaskStatus {
    /// `- [ ]` Unchecked
    Incomplete,
    /// `- [/]` In progress
    InProgress,
    /// `- [-]` Cancelled
    Cancelled,
    /// `- [x]` Done
    Completed,
}

/// Task priority levels (obsidian-tasks compatible).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize, Default)]
pub enum Priority {
    Highest,
    High,
    Medium,
    #[default]
    Normal,
    Low,
    Lowest,
}

/// A task extracted from an Obsidian markdown file.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Task {
    /// Task description (cleaned of metadata)
    pub description: String,
    /// Original raw text of the task line
    pub raw_text: String,
    /// Source note path (vault-relative)
    pub source_note: String,
    /// Task completion status
    pub status: TaskStatus,
    /// Due date
    pub due_date: Option<NaiveDate>,
    /// Scheduled date
    pub scheduled_date: Option<NaiveDate>,
    /// Start date
    pub start_date: Option<NaiveDate>,
    /// Completed date
    pub completed_date: Option<NaiveDate>,
    /// Priority
    pub priority: Priority,
    /// Tags (e.g. #work, #personal)
    pub tags: Vec<String>,
    /// Parent task breadcrumbs (heading hierarchy)
    pub parent_breadcrumbs: Vec<String>,
    /// Start date inherited from parent task
    pub parent_start_date: Option<NaiveDate>,
    /// 1-based task number in DFS order
    pub task_num: u32,
    /// Parent task number (for nested tasks)
    pub parent_task_num: Option<u32>,
}

/// Extract all tasks from a comrak AST document.
///
/// Assigns 1-based task numbers in DFS order, tracking parent-child relationships.
pub fn extract_tasks<'a>(source_path: &str, root: &'a AstNode<'a>) -> Vec<Task> {
    let mut tasks = Vec::new();
    let mut next_num: u32 = 1;
    extract_from_children(
        source_path,
        root,
        None, // parent_num
        &[],  // breadcrumbs
        None, // inherited_start
        &mut next_num,
        &mut tasks,
    );
    tasks
}

fn extract_from_children<'a>(
    source_path: &str,
    node: &'a AstNode<'a>,
    parent_num: Option<u32>,
    breadcrumbs: &[String],
    inherited_start: Option<NaiveDate>,
    next_num: &mut u32,
    tasks: &mut Vec<Task>,
) {
    for child in node.children() {
        let value = child.data.borrow().value.clone();
        match value {
            NodeValue::List(_) => {
                // List children are Item or TaskItem nodes
                for item in child.children() {
                    extract_from_item(
                        source_path,
                        item,
                        parent_num,
                        breadcrumbs,
                        inherited_start,
                        next_num,
                        tasks,
                    );
                }
            }
            _ => {
                // Recurse into non-list blocks to find nested lists
                extract_from_children(
                    source_path,
                    child,
                    parent_num,
                    breadcrumbs,
                    inherited_start,
                    next_num,
                    tasks,
                );
            }
        }
    }
}

fn extract_from_item<'a>(
    source_path: &str,
    item: &'a AstNode<'a>,
    parent_num: Option<u32>,
    breadcrumbs: &[String],
    inherited_start: Option<NaiveDate>,
    next_num: &mut u32,
    tasks: &mut Vec<Task>,
) {
    // Check if this item is a task (has TaskItem node or checkbox text)
    let (maybe_task_info, nested_children) = extract_task_from_item(item);

    match maybe_task_info {
        Some((status, text)) => {
            let props = parse_task_text(&text);
            let this_num = *next_num;
            *next_num += 1;

            // For completed tasks, only keep completed_date if actually completed
            let completed_date = if status == TaskStatus::Completed {
                props.completed_date
            } else {
                None
            };

            let task = Task {
                description: props.description.clone(),
                raw_text: text,
                source_note: source_path.to_string(),
                status,
                due_date: props.due_date,
                scheduled_date: props.scheduled_date,
                start_date: props.start_date,
                completed_date,
                priority: props.priority,
                tags: props.tags,
                parent_breadcrumbs: breadcrumbs.to_vec(),
                parent_start_date: inherited_start,
                task_num: this_num,
                parent_task_num: parent_num,
            };

            // Build breadcrumbs for children
            let mut child_breadcrumbs = breadcrumbs.to_vec();
            child_breadcrumbs.push(props.description);

            // Children inherit start date
            let child_start = task.start_date.or(inherited_start);

            tasks.push(task);

            // Process nested lists within this item
            for nested in nested_children {
                extract_from_children(
                    source_path,
                    nested,
                    Some(this_num),
                    &child_breadcrumbs,
                    child_start,
                    next_num,
                    tasks,
                );
            }
        }
        None => {
            // Not a task — still check for nested lists
            for nested in nested_children {
                extract_from_children(
                    source_path,
                    nested,
                    parent_num,
                    breadcrumbs,
                    inherited_start,
                    next_num,
                    tasks,
                );
            }
        }
    }
}

/// Extract task info from a list item node.
/// Returns (status, text) if it's a task, and a list of child nodes that may contain nested lists.
fn extract_task_from_item<'a>(
    item: &'a AstNode<'a>,
) -> (Option<(TaskStatus, String)>, Vec<&'a AstNode<'a>>) {
    let mut task_info = None;
    let mut nested_children = Vec::new();

    // comrak represents task items as NodeValue::TaskItem(Option<char>)
    // None = unchecked, Some('x'|'X') = completed, Some('/') = in-progress, Some('-') = cancelled
    let item_value = item.data.borrow().value.clone();
    if let NodeValue::TaskItem(checked) = item_value {
        let text = collect_item_text(item);
        let status = match checked {
            None => TaskStatus::Incomplete,
            Some('x') | Some('X') => TaskStatus::Completed,
            Some('/') => TaskStatus::InProgress,
            Some('-') => TaskStatus::Cancelled,
            _ => TaskStatus::Incomplete,
        };
        task_info = Some((status, text));
    }

    for child in item.children() {
        let value = child.data.borrow().value.clone();
        match value {
            NodeValue::Paragraph => {
                // For regular Item nodes (not TaskItem), check paragraph text
                if task_info.is_none() {
                    let text = collect_paragraph_text(child);
                    if let Some((status, task_text)) = try_parse_task_line(&text) {
                        task_info = Some((status, task_text));
                    }
                }
            }
            NodeValue::List(_) => {
                nested_children.push(child);
            }
            _ => {}
        }
    }

    (task_info, nested_children)
}

/// Try to parse a task line of the form `[ ] text`, `[x] text`, `[/] text`, `[-] text`.
fn try_parse_task_line(line: &str) -> Option<(TaskStatus, String)> {
    let trimmed = line.trim();

    // Check for checkbox markers at the start
    let checkbox_patterns: &[(&str, TaskStatus)] = &[
        ("[ ] ", TaskStatus::Incomplete),
        ("[x] ", TaskStatus::Completed),
        ("[X] ", TaskStatus::Completed),
        ("[/] ", TaskStatus::InProgress),
        ("[-] ", TaskStatus::Cancelled),
        // Unicode checkboxes
        ("\u{2610} ", TaskStatus::Incomplete), // ☐
        ("\u{2612} ", TaskStatus::Completed),  // ☒
    ];

    for (pattern, status) in checkbox_patterns {
        if let Some(stripped) = trimmed.strip_prefix(pattern) {
            return Some((*status, stripped.to_string()));
        }
    }

    None
}

/// Collect text content from a paragraph node.
fn collect_paragraph_text<'a>(node: &'a AstNode<'a>) -> String {
    let mut text = String::new();
    collect_text_recursive(node, &mut text);
    text
}

/// Collect text content from an item node (skipping list children).
fn collect_item_text<'a>(item: &'a AstNode<'a>) -> String {
    let mut text = String::new();
    for child in item.children() {
        match &child.data.borrow().value {
            NodeValue::List(_) => {} // Skip nested lists
            _ => collect_text_recursive(child, &mut text),
        }
    }
    text
}

fn collect_text_recursive<'a>(node: &'a AstNode<'a>, buf: &mut String) {
    match &node.data.borrow().value {
        NodeValue::Text(t) => buf.push_str(t),
        NodeValue::SoftBreak | NodeValue::LineBreak => buf.push(' '),
        NodeValue::Code(c) => buf.push_str(&c.literal),
        _ => {
            for child in node.children() {
                collect_text_recursive(child, buf);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use comrak::Arena;

    fn parse_and_extract(markdown: &str) -> Vec<Task> {
        let arena = Arena::new();
        let parsed = crate::markdown::parse_markdown(&arena, markdown);
        extract_tasks("test.md", parsed.root)
    }

    #[test]
    fn test_no_tasks() {
        let tasks = parse_and_extract("# Hello\n\nJust some text.");
        assert!(tasks.is_empty());
    }

    #[test]
    fn test_basic_task() {
        let tasks = parse_and_extract("- [ ] Buy groceries\n");
        assert_eq!(tasks.len(), 1);
        assert_eq!(tasks[0].status, TaskStatus::Incomplete);
        assert!(tasks[0].description.contains("Buy groceries"));
        assert_eq!(tasks[0].task_num, 1);
    }

    #[test]
    fn test_completed_task() {
        let tasks = parse_and_extract("- [x] Done task\n");
        assert_eq!(tasks.len(), 1);
        assert_eq!(tasks[0].status, TaskStatus::Completed);
    }

    #[test]
    fn test_multiple_tasks() {
        let tasks = parse_and_extract("- [ ] First\n- [x] Second\n- [/] Third\n");
        assert_eq!(tasks.len(), 3);
        assert_eq!(tasks[0].task_num, 1);
        assert_eq!(tasks[1].task_num, 2);
        assert_eq!(tasks[2].task_num, 3);
        assert_eq!(tasks[2].status, TaskStatus::InProgress);
    }

    #[test]
    fn test_task_with_metadata() {
        let tasks = parse_and_extract("- [ ] Fix bug 📅 2025-03-01 ⏫\n");
        assert_eq!(tasks.len(), 1);
        assert!(tasks[0].description.contains("Fix bug"));
        assert_eq!(
            tasks[0].due_date,
            Some(NaiveDate::from_ymd_opt(2025, 3, 1).unwrap())
        );
        assert_eq!(tasks[0].priority, Priority::High);
    }
}
