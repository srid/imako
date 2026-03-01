//! Folder tree sidebar component.

use crate::Route;
use dioxus::prelude::*;
use ob::FolderNode;

#[component]
pub fn FolderTree(node: FolderNode, base_path: String, selected_path: Option<String>) -> Element {
    rsx! {
        div {
            class: "flex flex-col gap-0.5",

            // Render subfolders
            for (name, subnode) in &node.subfolders {
                {
                    let folder_path = if base_path.is_empty() {
                        name.clone()
                    } else {
                        format!("{}/{}", base_path, name)
                    };
                    let is_selected = selected_path.as_deref() == Some(folder_path.as_str());
                    let folder_path_recurse = folder_path.clone();
                    let segments: Vec<String> = folder_path.split('/').map(String::from).collect();

                    rsx! {
                        details {
                            open: true,
                            class: "group/folder",
                            summary {
                                class: if is_selected {
                                    "list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-semibold select-none transition-colors text-indigo-600 bg-indigo-50 rounded-md px-2 -mx-2"
                                } else {
                                    "list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-semibold select-none transition-colors text-stone-700 hover:text-indigo-600"
                                },
                                Link {
                                    to: Route::VaultPath { path: segments },
                                    class: "flex items-center gap-2 w-full",
                                    span { class: "text-indigo-500", "📁" }
                                    span { "{name}" }
                                }
                            }
                            div {
                                class: "pl-6 mt-0.5",
                                FolderTree {
                                    node: subnode.clone(),
                                    base_path: folder_path_recurse,
                                    selected_path: selected_path.clone(),
                                }
                            }
                        }
                    }
                }
            }

            // Render files
            for (filename, entry) in &node.files {
                {
                    let file_path = entry.path.to_string_lossy().to_string();
                    let is_selected = selected_path.as_deref() == Some(file_path.as_str());
                    let segments: Vec<String> = file_path.split('/').map(String::from).collect();

                    rsx! {
                        Link {
                            to: Route::VaultPath { path: segments },
                            class: if is_selected {
                                "w-full text-left py-1.5 flex items-center gap-2 text-sm transition-colors rounded-md text-indigo-600 bg-indigo-50 font-medium px-2 -mx-2"
                            } else {
                                "w-full text-left py-1.5 flex items-center gap-2 text-sm transition-colors rounded-md text-stone-600 hover:text-indigo-600"
                            },
                            span { class: "w-4 h-4" } // spacer
                            span { class: "text-stone-400", "📄" }
                            span { class: "truncate", "{filename}" }
                        }
                    }
                }
            }
        }
    }
}
