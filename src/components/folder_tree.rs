//! Folder tree sidebar component.

use dioxus::prelude::*;
use ob::FolderNode;

#[component]
pub fn FolderTree(
    node: FolderNode,
    base_path: String,
    selected_path: Option<String>,
    on_select: EventHandler<Option<String>>,
) -> Element {
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
                    let folder_path_click = folder_path.clone();
                    let folder_path_recurse = folder_path.clone();

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
                                onclick: move |_| on_select.call(Some(folder_path_click.clone())),
                                span {
                                    class: "w-4 h-4 flex items-center justify-center text-stone-400 transition-transform",
                                    "▸"
                                }
                                span { class: "text-indigo-500", "📁" }
                                span { "{name}" }
                            }
                            div {
                                class: "pl-6 mt-0.5",
                                FolderTree {
                                    node: subnode.clone(),
                                    base_path: folder_path_recurse,
                                    selected_path: selected_path.clone(),
                                    on_select: move |p| on_select.call(p),
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
                    let path_click = file_path.clone();

                    rsx! {
                        button {
                            class: if is_selected {
                                "w-full text-left py-1.5 flex items-center gap-2 text-sm transition-colors rounded-md text-indigo-600 bg-indigo-50 font-medium px-2 -mx-2"
                            } else {
                                "w-full text-left py-1.5 flex items-center gap-2 text-sm transition-colors rounded-md text-stone-600 hover:text-indigo-600"
                            },
                            onclick: move |_| on_select.call(Some(path_click.clone())),
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
