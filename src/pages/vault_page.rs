//! Main vault page — two-pane layout with sidebar and detail view.

use crate::components::folder_tree::FolderTree;
use crate::components::header::Header;
use crate::components::note_view::NoteView;
use crate::shared::FolderTreeData;
use crate::{get_folder_tree, get_note};
use dioxus::prelude::*;

#[component]
pub fn VaultPage(path: Option<String>) -> Element {
    // Fetch folder tree data
    let folder_tree = use_server_future(get_folder_tree)?;

    rsx! {
        div {
            class: "flex h-screen bg-white text-stone-800 font-sans antialiased",

            // Sidebar
            aside {
                class: "w-72 shrink-0 border-r border-stone-200 bg-stone-50 overflow-y-auto p-4",

                match &*folder_tree.read() {
                    Some(Ok(data)) => rsx! {
                        Header { info: data.info.clone() }
                        FolderTree {
                            node: data.tree.clone(),
                            base_path: String::new(),
                            selected_path: path.clone(),
                        }
                    },
                    Some(Err(e)) => rsx! {
                        div {
                            class: "text-red-600 text-sm p-4",
                            "Error loading vault: {e}"
                        }
                    },
                    None => rsx! {
                        div {
                            class: "flex items-center justify-center p-8",
                            div {
                                class: "animate-pulse text-stone-400 text-sm",
                                "Loading vault…"
                            }
                        }
                    },
                }
            }

            // Main content
            main {
                class: "flex-1 overflow-y-auto p-8",
                match &path {
                    Some(file_path) if file_path.ends_with(".md") => rsx! {
                        NoteDetail { path: file_path.clone() }
                    },
                    Some(folder_path) => rsx! {
                        FolderDetail {
                            path: folder_path.clone(),
                            tree_data: folder_tree.read().as_ref().and_then(|r| r.as_ref().ok().cloned()),
                        }
                    },
                    None => rsx! {
                        RootView {
                            tree_data: folder_tree.read().as_ref().and_then(|r| r.as_ref().ok().cloned()),
                        }
                    },
                }
            }
        }
    }
}

/// Renders the detail view for a specific note file.
#[component]
fn NoteDetail(path: String) -> Element {
    let note_data = use_resource(move || {
        let p = path.clone();
        async move { get_note(p).await }
    });

    let binding = note_data.read();
    match &*binding {
        Some(Ok(data)) => rsx! {
            NoteView { data: data.clone() }
        },
        Some(Err(e)) => {
            let msg = format!("{e}");
            rsx! {
                div {
                    class: "text-red-600 text-sm",
                    "Error loading note: {msg}"
                }
            }
        }
        None => rsx! {
            div {
                class: "animate-pulse text-stone-400",
                "Loading note…"
            }
        },
    }
}

/// Renders the detail view for a folder — lists its contents.
#[component]
fn FolderDetail(path: String, tree_data: Option<FolderTreeData>) -> Element {
    let folder_name = path.split('/').next_back().unwrap_or(&path);

    rsx! {
        div {
            class: "space-y-6",
            h2 {
                class: "text-lg font-semibold text-stone-700 flex items-center gap-2",
                span { "📁" }
                span { "{folder_name}" }
            }

            if let Some(data) = &tree_data {
                { render_folder_contents(&data.tree, &path) }
            }
        }
    }
}

/// Navigate the tree to find and render a specific folder's contents.
fn render_folder_contents(root: &ob::FolderNode, target_path: &str) -> Element {
    // Walk the tree to find the target folder
    let parts: Vec<&str> = target_path.split('/').collect();
    let mut current = root;

    for part in &parts {
        // Try to find this part in subfolders
        // Handle flattened names (e.g., "A/B")
        let mut found = false;
        for (name, node) in &current.subfolders {
            if name == *part || name.ends_with(&format!("/{}", part)) || *part == name.as_str() {
                current = node;
                found = true;
                break;
            }
        }
        if !found {
            // Check if the full path matches a flattened key
            for (name, node) in &current.subfolders {
                if target_path.starts_with(name.as_str()) || name == target_path {
                    current = node;
                    found = true;
                    break;
                }
            }
            if !found {
                return rsx! {
                    p { class: "text-stone-400 text-sm italic", "Folder not found." }
                };
            }
        }
    }

    rsx! {
        div {
            class: "space-y-2",
            // Subfolders
            for (name, _) in &current.subfolders {
                {
                    let full_path = format!("{}/{}", target_path, name);
                    rsx! {
                        a {
                            class: "flex items-center gap-2 py-2 px-3 rounded-lg hover:bg-stone-50 text-stone-700 cursor-pointer transition-colors",
                            href: "/p/{full_path}",
                            span { "📁" }
                            span { class: "font-medium", "{name}" }
                        }
                    }
                }
            }
            // Files
            for (filename, entry) in &current.files {
                {
                    let file_path = entry.path.to_string_lossy().to_string();
                    rsx! {
                        a {
                            class: "flex items-center gap-2 py-2 px-3 rounded-lg hover:bg-stone-50 text-stone-600 cursor-pointer transition-colors",
                            href: "/p/{file_path}",
                            span { "📄" }
                            span { "{filename}" }
                        }
                    }
                }
            }
        }
    }
}

/// Root view — shows vault overview.
#[component]
fn RootView(tree_data: Option<FolderTreeData>) -> Element {
    rsx! {
        div {
            class: "space-y-6",
            h2 {
                class: "text-lg font-semibold text-stone-700",
                "Vault"
            }

            if let Some(data) = &tree_data {
                div {
                    class: "space-y-2",
                    // Top-level subfolders
                    for (name, _) in &data.tree.subfolders {
                        a {
                            class: "flex items-center gap-2 py-2 px-3 rounded-lg hover:bg-stone-50 text-stone-700 cursor-pointer transition-colors",
                            href: "/p/{name}",
                            span { "📁" }
                            span { class: "font-medium", "{name}" }
                        }
                    }
                    // Top-level files
                    for (filename, entry) in &data.tree.files {
                        {
                            let file_path = entry.path.to_string_lossy().to_string();
                            rsx! {
                                a {
                                    class: "flex items-center gap-2 py-2 px-3 rounded-lg hover:bg-stone-50 text-stone-600 cursor-pointer transition-colors",
                                    href: "/p/{file_path}",
                                    span { "📄" }
                                    span { "{filename}" }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}
