//! Hierarchical folder tree builder.
//!
//! Builds a tree from flat note paths, annotated with tasks
//! and daily note dates. Supports collapsing single-child folders.

use std::collections::BTreeMap;

use chrono::NaiveDate;
use ob::daily_notes::{DailyNote, DailyNotesConfig};
use ob::note::Note;
use ob::task::{Task, TaskStatus};
use serde::{Deserialize, Serialize};

/// A node in the folder tree.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct FolderNode {
    /// Subfolders keyed by name
    pub subfolders: BTreeMap<String, FolderNode>,
    /// Files keyed by filename, with their tasks
    pub files: BTreeMap<String, Vec<Task>>,
    /// Daily note dates (only set on the daily notes folder)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub daily_note_dates: Option<BTreeMap<String, NaiveDate>>,
}

impl FolderNode {
    fn empty() -> Self {
        FolderNode {
            subfolders: BTreeMap::new(),
            files: BTreeMap::new(),
            daily_note_dates: None,
        }
    }
}

/// Build a folder tree from notes.
pub fn build_folder_tree(notes: &[Note]) -> FolderNode {
    let mut root = FolderNode::empty();

    for note in notes {
        let parts: Vec<&str> = note.path.split('/').collect();
        insert_path(&mut root, &parts, note.tasks.clone());
    }

    root
}

fn insert_path(node: &mut FolderNode, parts: &[&str], tasks: Vec<Task>) {
    match parts {
        [] => {}
        [filename] => {
            node.files.insert(filename.to_string(), tasks);
        }
        [folder, rest @ ..] => {
            let subfolder = node
                .subfolders
                .entry(folder.to_string())
                .or_insert_with(FolderNode::empty);
            insert_path(subfolder, rest, tasks);
        }
    }
}

/// Annotate the daily notes folder with parsed dates.
pub fn annotate_daily_notes(
    root: &mut FolderNode,
    daily_notes: &[DailyNote],
    config: &DailyNotesConfig,
) {
    let date_map: BTreeMap<String, NaiveDate> = daily_notes
        .iter()
        .map(|dn| {
            let filename = dn
                .note_path
                .rsplit('/')
                .next()
                .unwrap_or(&dn.note_path)
                .to_string();
            (filename, dn.day)
        })
        .collect();

    // Find the daily notes folder in the tree
    if config.folder == "." {
        root.daily_note_dates = Some(date_map);
    } else if let Some(subfolder) = root.subfolders.get_mut(&config.folder) {
        subfolder.daily_note_dates = Some(date_map);
    }
}

/// Check if a task is due (incomplete/in-progress and due date <= today).
pub fn is_task_due(today: NaiveDate, task: &Task) -> bool {
    matches!(task.status, TaskStatus::Incomplete | TaskStatus::InProgress)
        && task.due_date.is_some_and(|d| d <= today)
}

/// Recursively check if a folder node has any due tasks.
pub fn has_due_tasks(today: NaiveDate, node: &FolderNode) -> bool {
    let files_due = node
        .files
        .values()
        .any(|tasks| tasks.iter().any(|t| is_task_due(today, t)));
    let subfolders_due = node
        .subfolders
        .values()
        .any(|sub| has_due_tasks(today, sub));
    files_due || subfolders_due
}

/// Recursively flatten folder ancestry where possible.
///
/// Merges a parent with its only child folder (no files) into a single node
/// with a combined name (e.g. "Projects/Team").
pub fn flatten_tree(node: &mut FolderNode) {
    // First, recursively flatten children
    for sub in node.subfolders.values_mut() {
        flatten_tree(sub);
    }

    // Then collapse single-child folders
    let keys: Vec<String> = node.subfolders.keys().cloned().collect();
    for name in keys {
        let should_collapse = {
            let sub = &node.subfolders[&name];
            sub.files.is_empty() && sub.subfolders.len() == 1
        };
        if should_collapse {
            let sub = node.subfolders.remove(&name).unwrap();
            let (child_name, child_node) = sub.subfolders.into_iter().next().unwrap();
            let merged_name = format!("{}/{}", name, child_name);
            node.subfolders.insert(merged_name, child_node);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_build_folder_tree() {
        let notes = vec![
            Note::from_string("Projects/Frontend.md", "# Frontend"),
            Note::from_string("Projects/Backend.md", "# Backend"),
            Note::from_string("Notes/Ideas.md", "# Ideas"),
        ];

        let tree = build_folder_tree(&notes);
        assert!(tree.subfolders.contains_key("Projects"));
        assert!(tree.subfolders.contains_key("Notes"));
        assert_eq!(tree.subfolders["Projects"].files.len(), 2);
    }

    #[test]
    fn test_flatten_tree() {
        let notes = vec![Note::from_string("A/B/C/file.md", "# Content")];

        let mut tree = build_folder_tree(&notes);
        flatten_tree(&mut tree);

        // A/B/C should be collapsed since A and B have no files and one subfolder
        assert!(tree.subfolders.contains_key("A/B/C"));
    }
}
