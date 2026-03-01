//! Folder tree construction from vault notes.
//!
//! Builds a hierarchical tree of folders and files from the flat
//! note path map. Supports collapsing single-child empty folders
//! (e.g., "A/B" when B has no files and exactly one subfolder).

use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};
use std::path::{Path, PathBuf};

/// A node in the folder tree hierarchy.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FolderNode {
    /// Subfolders, keyed by folder name.
    pub subfolders: BTreeMap<String, FolderNode>,

    /// Files at this level, keyed by filename.
    pub files: BTreeMap<String, FileEntry>,
}

/// A file entry in the folder tree.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct FileEntry {
    /// Vault-relative path to this file.
    pub path: PathBuf,
}

impl FolderNode {
    fn new() -> Self {
        Self {
            subfolders: BTreeMap::new(),
            files: BTreeMap::new(),
        }
    }
}

/// Build a folder tree from a set of vault-relative note paths.
pub fn build_folder_tree<'a>(paths: impl IntoIterator<Item = &'a PathBuf>) -> FolderNode {
    let mut root = FolderNode::new();

    for path in paths {
        insert_path(&mut root, path);
    }

    flatten_tree(&mut root);
    root
}

/// Insert a file path into the folder tree.
fn insert_path(root: &mut FolderNode, path: &Path) {
    let components: Vec<&str> = path
        .components()
        .filter_map(|c| c.as_os_str().to_str())
        .collect();

    if components.is_empty() {
        return;
    }

    let mut current = root;

    // Navigate to the parent folder, creating intermediate folders as needed
    for &component in &components[..components.len() - 1] {
        current = current
            .subfolders
            .entry(component.to_string())
            .or_insert_with(FolderNode::new);
    }

    // Insert the file at the leaf
    let filename = components[components.len() - 1].to_string();
    current.files.insert(
        filename,
        FileEntry {
            path: path.to_path_buf(),
        },
    );
}

/// Recursively flatten single-child empty folders.
///
/// If a folder has no files and exactly one subfolder, merge them:
/// "A" containing only "B" becomes "A/B".
fn flatten_tree(node: &mut FolderNode) {
    // First, recursively flatten children
    for subfolder in node.subfolders.values_mut() {
        flatten_tree(subfolder);
    }

    // Then collapse single-child empty folders at this level
    let keys: Vec<String> = node.subfolders.keys().cloned().collect();
    let mut replacements: Vec<(String, String, FolderNode)> = Vec::new();

    for key in &keys {
        if let Some(child) = node.subfolders.get(key)
            && child.files.is_empty()
            && child.subfolders.len() == 1
        {
            let (child_name, grandchild) = child.subfolders.iter().next().unwrap();
            let merged_name = format!("{}/{}", key, child_name);
            replacements.push((key.clone(), merged_name, grandchild.clone()));
        }
    }

    for (old_key, new_key, replacement) in replacements {
        node.subfolders.remove(&old_key);
        node.subfolders.insert(new_key, replacement);
    }
}

/// Build a folder tree from a HashMap of notes.
pub fn build_folder_tree_from_notes(notes: &HashMap<PathBuf, crate::Note>) -> FolderNode {
    let paths: Vec<&PathBuf> = notes.keys().collect();
    build_folder_tree(paths)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn simple_tree() {
        let paths = vec![
            PathBuf::from("Notes/Welcome.md"),
            PathBuf::from("Notes/Tasks.md"),
            PathBuf::from("Daily/2024-01-01.md"),
        ];
        let tree = build_folder_tree(&paths);

        assert_eq!(tree.subfolders.len(), 2);
        assert!(tree.subfolders.contains_key("Notes"));
        assert!(tree.subfolders.contains_key("Daily"));

        let notes_folder = &tree.subfolders["Notes"];
        assert_eq!(notes_folder.files.len(), 2);
        assert!(notes_folder.files.contains_key("Welcome.md"));
        assert!(notes_folder.files.contains_key("Tasks.md"));
    }

    #[test]
    fn flattens_single_child_folders() {
        let paths = vec![PathBuf::from("A/B/C/file.md")];
        let tree = build_folder_tree(&paths);

        // A/B/C should be flattened since each has exactly one child
        assert_eq!(tree.subfolders.len(), 1);
        let key = tree.subfolders.keys().next().unwrap();
        assert_eq!(key, "A/B/C");

        let leaf = &tree.subfolders[key];
        assert_eq!(leaf.files.len(), 1);
        assert!(leaf.files.contains_key("file.md"));
    }

    #[test]
    fn no_flatten_when_multiple_children() {
        let paths = vec![PathBuf::from("A/B/file1.md"), PathBuf::from("A/C/file2.md")];
        let tree = build_folder_tree(&paths);

        // A has two children so shouldn't be flattened
        assert_eq!(tree.subfolders.len(), 1);
        assert!(tree.subfolders.contains_key("A"));
        let a = &tree.subfolders["A"];
        assert_eq!(a.subfolders.len(), 2);
    }

    #[test]
    fn root_level_files() {
        let paths = vec![PathBuf::from("README.md")];
        let tree = build_folder_tree(&paths);

        assert_eq!(tree.files.len(), 1);
        assert!(tree.files.contains_key("README.md"));
        assert!(tree.subfolders.is_empty());
    }
}
