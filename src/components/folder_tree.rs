//! Recursive folder tree sidebar component.

use dioxus::prelude::*;
use imako_core::folder_tree::FolderNode;

/// Recursive folder tree component.
#[component]
pub fn FolderTree(node: FolderNode, base_path: String, selected_path: Option<String>) -> Element {
    let mut items: Vec<(String, TreeEntry)> = Vec::new();

    // Subfolders first
    for (name, sub) in node.subfolders.iter() {
        items.push((name.clone(), TreeEntry::Folder(sub.clone())));
    }
    // Then files
    for (name, _) in node.files.iter() {
        items.push((name.clone(), TreeEntry::File));
    }

    rsx! {
        div { class: "space-y-0.5", "data-testid": "folder-tree",
            for (name, entry) in items.iter() {
                {
                    let child_path = if base_path.is_empty() {
                        name.clone()
                    } else {
                        format!("{}/{}", base_path, name)
                    };
                    let is_selected = selected_path.as_ref() == Some(&child_path);

                    match entry {
                        TreeEntry::Folder(subnode) => {
                            rsx! {
                                details { open: true, "data-testid": "folder-node",
                                    summary {
                                        class: if is_selected {
                                            "list-none cursor-pointer py-1 px-2 -mx-2 flex items-center gap-2 text-sm rounded-md transition-colors text-amber-600 dark:text-amber-400 bg-amber-50 dark:bg-amber-900/20 select-none"
                                        } else {
                                            "list-none cursor-pointer py-1 flex items-center gap-2 text-sm rounded-md transition-colors text-stone-600 dark:text-stone-300 hover:text-amber-600 dark:hover:text-amber-400 select-none"
                                        },
                                        Link {
                                            to: crate::Route::VaultPath { path: urlencoding::encode(&child_path).to_string() },
                                            class: "flex items-center gap-2 w-full",
                                            "data-testid": "folder-label",
                                            span { class: "text-amber-500 flex-shrink-0", "📁" }
                                            span { class: "truncate", "{name}" }
                                        }
                                    }
                                    div { class: "pl-4",
                                        FolderTree {
                                            node: subnode.clone(),
                                            base_path: child_path,
                                            selected_path: selected_path.clone(),
                                        }
                                    }
                                }
                            }
                        }
                        TreeEntry::File => {
                            rsx! {
                                Link { "data-testid": "file-node",
                                    to: crate::Route::VaultPath { path: urlencoding::encode(&child_path).to_string() },
                                    class: if is_selected {
                                        "block py-1 px-2 -mx-2 flex items-center gap-2 text-sm rounded-md transition-colors text-amber-600 dark:text-amber-400 bg-amber-50 dark:bg-amber-900/20"
                                    } else {
                                        "block py-1 flex items-center gap-2 text-sm rounded-md transition-colors text-stone-600 dark:text-stone-300 hover:text-amber-600 dark:hover:text-amber-400"
                                    },
                                    span { class: "text-stone-400 dark:text-stone-500 flex-shrink-0", "📄" }
                                    span { class: "truncate", "{name}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

enum TreeEntry {
    Folder(FolderNode),
    File,
}
