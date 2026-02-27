//! Main vault page — sidebar + detail panel.

use dioxus::prelude::*;
use imako_core::folder_tree::FolderNode;
use imako_core::protocol::VaultTreeData;
use ob::task::{Task, TaskStatus};

use super::folder_tree::FolderTree;
use super::task_item::TaskItem;
use crate::server;

/// The main vault page component.
#[component]
pub fn VaultPage(selected_path: Option<String>) -> Element {
    let vault_data = use_server_future(server::get_vault_tree)?;

    rsx! {
        div { class: "min-h-screen bg-stone-50/50 dark:bg-stone-900 text-stone-800 dark:text-stone-200",
            div { class: "max-w-6xl mx-auto my-8 px-4",
                div { class: "bg-white dark:bg-stone-950 rounded-2xl shadow-sm border border-stone-200 dark:border-stone-700 p-6 sm:p-8",
                    // Header
                    Header {}

                    // Main layout: sidebar + detail
                    match &*vault_data.read() {
                        Some(Ok(data)) => rsx! {
                            div { class: "grid grid-cols-1 md:grid-cols-[minmax(200px,1fr)_2fr] gap-6 min-h-[calc(100vh-12rem)]",
                                // Sidebar
                                Sidebar {
                                    data: data.clone(),
                                    selected_path: selected_path.clone(),
                                }
                                // Main panel
                                MainPanel {
                                    data: data.clone(),
                                    selected_path: selected_path.clone(),
                                }
                            }
                        },
                        Some(Err(e)) => rsx! {
                            p { class: "text-red-500", "Error: {e}" }
                        },
                        None => rsx! {
                            div { class: "flex items-center justify-center min-h-[50vh]",
                                div { class: "text-center",
                                    div { class: "inline-block w-8 h-8 border-4 border-amber-500 border-t-transparent rounded-full animate-spin mb-4" }
                                    p { class: "text-stone-500 dark:text-stone-400", "Loading vault…" }
                                }
                            }
                        },
                    }
                }
            }
        }
    }
}

/// App header.
#[component]
fn Header() -> Element {
    rsx! {
        header { class: "flex items-center justify-between mb-6 pb-4 border-b border-stone-200 dark:border-stone-700",
            h1 { class: "text-xl font-bold text-stone-700 dark:text-stone-200",
                "いまここ"
            }
        }
    }
}

/// Left sidebar with folder tree.
#[component]
fn Sidebar(data: VaultTreeData, selected_path: Option<String>) -> Element {
    rsx! {
        aside { class: "border-r border-stone-200 dark:border-stone-700 pr-4 overflow-y-auto md:max-h-[calc(100vh-12rem)]",
            // Root vault button
            button {
                class: if selected_path.is_none() {
                    "w-full text-left py-2 mb-2 text-sm font-semibold transition-colors rounded-md text-amber-600 dark:text-amber-400 bg-amber-50 dark:bg-amber-900/20 px-2 -mx-2"
                } else {
                    "w-full text-left py-2 mb-2 text-sm font-semibold transition-colors rounded-md text-stone-700 dark:text-stone-200 hover:text-amber-600 dark:hover:text-amber-400"
                },
                onclick: move |_| {
                    use_navigator().push(crate::Route::VaultRoot {});
                },
                span { class: "flex items-center gap-2",
                    span { class: "text-amber-500", "📁" }
                    "{data.info.vault_name}"
                }
            }

            // Folder tree
            FolderTree {
                node: data.tree.clone(),
                base_path: String::new(),
                selected_path: selected_path.clone(),
            }
        }
    }
}

/// Main content panel.
#[component]
fn MainPanel(data: VaultTreeData, selected_path: Option<String>) -> Element {
    match selected_path {
        None => {
            // Root: show all tasks
            rsx! {
                main { class: "overflow-y-auto md:max-h-[calc(100vh-12rem)]",
                    TaskGroupTree {
                        node: data.tree.clone(),
                        base_path: String::new(),
                        today: data.info.today.to_string(),
                    }
                }
            }
        }
        Some(ref path) => {
            let subtree = get_subtree(&data.tree, path);
            match subtree {
                Some(SubtreeResult::Folder(name, node)) => {
                    rsx! {
                        main { class: "overflow-y-auto md:max-h-[calc(100vh-12rem)]",
                            FolderView {
                                name: name,
                                node: node,
                                base_path: path.clone(),
                                today: data.info.today.to_string(),
                            }
                        }
                    }
                }
                Some(SubtreeResult::File(name, tasks)) => {
                    rsx! {
                        main { class: "overflow-y-auto md:max-h-[calc(100vh-12rem)]",
                            FileView {
                                name: name,
                                path: path.clone(),
                                tasks: tasks,
                                today: data.info.today.to_string(),
                            }
                        }
                    }
                }
                None => {
                    rsx! {
                        main { class: "overflow-y-auto md:max-h-[calc(100vh-12rem)]",
                            p { class: "text-stone-400 dark:text-stone-500 text-sm", "Not found" }
                        }
                    }
                }
            }
        }
    }
}

/// Subtree lookup result.
enum SubtreeResult {
    Folder(String, FolderNode),
    File(String, Vec<Task>),
}

fn get_subtree(root: &FolderNode, path: &str) -> Option<SubtreeResult> {
    let parts: Vec<&str> = path.split('/').filter(|s| !s.is_empty()).collect();
    let mut current = root;

    for (i, part) in parts.iter().enumerate() {
        let is_last = i == parts.len() - 1;

        if is_last {
            // Check if it's a file
            if let Some(tasks) = current.files.get(*part) {
                return Some(SubtreeResult::File(part.to_string(), tasks.clone()));
            }
        }

        if let Some(subfolder) = current.subfolders.get(*part) {
            current = subfolder;
            if is_last {
                return Some(SubtreeResult::Folder(part.to_string(), current.clone()));
            }
        } else {
            return None;
        }
    }

    None
}

/// Recursive task group tree — renders tasks grouped by file/folder.
#[component]
fn TaskGroupTree(node: FolderNode, base_path: String, today: String) -> Element {
    let files: Vec<(String, Vec<Task>)> = node
        .files
        .iter()
        .filter(|(_, tasks)| tasks.iter().any(|t| is_task_visible(t, &today)))
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    let folders: Vec<(String, FolderNode)> = node
        .subfolders
        .iter()
        .filter(|(_, sub)| count_visible_tasks(sub, &today) > 0)
        .map(|(k, v)| (k.clone(), v.clone()))
        .collect();

    rsx! {
        div { class: "space-y-2",
            // Files with tasks
            for (filename, tasks) in files.iter() {
                {
                    let visible: Vec<Task> = tasks.iter()
                        .filter(|t| is_task_visible(t, &today))
                        .cloned()
                        .collect();
                    let _file_path = if base_path.is_empty() {
                        filename.clone()
                    } else {
                        format!("{}/{}", base_path, filename)
                    };
                    rsx! {
                        div { "data-testid": "file-tasks-group",
                            details { open: true,
                                summary { class: "list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-medium text-stone-700 dark:text-stone-200 hover:text-amber-600 dark:hover:text-amber-400 select-none",
                                    span { class: "text-stone-400 dark:text-stone-500", "📄" }
                                    span { class: "truncate", "{filename}" }
                                    span { class: "text-xs text-stone-400 dark:text-stone-500 ml-auto flex-shrink-0",
                                        "{visible.len()}"
                                    }
                                }
                                div { class: "pl-6 flex flex-col",
                                    for task in visible.iter() {
                                        TaskItem { task: task.clone(), today: today.clone() }
                                    }
                                }
                            }
                        }
                    }
                }
            }

            // Subfolder groups
            for (name, subnode) in folders.iter() {
                {
                    let folder_path = if base_path.is_empty() {
                        name.clone()
                    } else {
                        format!("{}/{}", base_path, name)
                    };
                    let task_count = count_visible_tasks(subnode, &today);
                    rsx! {
                        div { "data-testid": "folder-tasks-group",
                            details { open: true,
                                summary { class: "list-none cursor-pointer py-1.5 flex items-center gap-2 text-sm font-semibold text-stone-700 dark:text-stone-200 hover:text-amber-600 dark:hover:text-amber-400 select-none",
                                    span { class: "text-amber-500", "📁" }
                                    span { class: "truncate", "{name}" }
                                    span { class: "text-xs text-stone-400 dark:text-stone-500 ml-auto flex-shrink-0",
                                        "{task_count}"
                                    }
                                }
                                div { class: "pl-6 flex flex-col gap-1",
                                    TaskGroupTree {
                                        node: subnode.clone(),
                                        base_path: folder_path,
                                        today: today.clone(),
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// Folder view: task tree + contents listing.
#[component]
fn FolderView(name: String, node: FolderNode, base_path: String, today: String) -> Element {
    let subfolders: Vec<String> = node.subfolders.keys().cloned().collect();
    let files: Vec<String> = node.files.keys().cloned().collect();

    rsx! {
        div { class: "space-y-6",
            // Tasks section
            div { "data-testid": "folder-task-view", class: "space-y-4",
                TaskGroupTree { node: node.clone(), base_path: base_path.clone(), today: today.clone() }
            }

            // Folder contents listing
            if !subfolders.is_empty() || !files.is_empty() {
                div { "data-testid": "folder-contents",
                    h3 { class: "text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3",
                        "Contents"
                    }
                    div { class: "grid gap-1",
                        for folder_name in subfolders.iter() {
                            {
                                let child_path = if base_path.is_empty() {
                                    folder_name.clone()
                                } else {
                                    format!("{}/{}", base_path, folder_name)
                                };
                                rsx! {
                                    Link {
                                        to: crate::Route::VaultPath { path: urlencoding::encode(&child_path).to_string() },
                                        class: "w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-amber-50 dark:hover:bg-amber-900/20 text-stone-700 dark:text-stone-200 hover:text-amber-600 dark:hover:text-amber-400",
                                        "data-testid": "folder-contents-folder",
                                        span { class: "text-amber-500", "📁" }
                                        span { class: "truncate", "{folder_name}" }
                                    }
                                }
                            }
                        }
                        for file_name in files.iter() {
                            {
                                let child_path = if base_path.is_empty() {
                                    file_name.clone()
                                } else {
                                    format!("{}/{}", base_path, file_name)
                                };
                                rsx! {
                                    Link {
                                        to: crate::Route::VaultPath { path: urlencoding::encode(&child_path).to_string() },
                                        class: "w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-stone-100 dark:hover:bg-stone-800 text-stone-600 dark:text-stone-300 hover:text-amber-600 dark:hover:text-amber-400",
                                        "data-testid": "folder-contents-file",
                                        span { class: "text-stone-400 dark:text-stone-500", "📄" }
                                        span { class: "truncate", "{file_name}" }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

/// File detail view: tasks + note content + backlinks.
#[component]
fn FileView(name: String, path: String, tasks: Vec<Task>, today: String) -> Element {
    let path_clone = path.clone();
    let note_data = use_server_future(move || {
        let p = path_clone.clone();
        async move { server::get_note(p).await }
    })?;

    let visible_tasks: Vec<Task> = tasks
        .iter()
        .filter(|t| is_task_visible(t, &today))
        .cloned()
        .collect();

    rsx! {
        div { class: "space-y-6",
            // File header
            div { class: "flex items-center gap-3",
                h2 { class: "text-lg font-semibold text-stone-700 dark:text-stone-200",
                    "{name}"
                }
            }

            // Tasks
            if !visible_tasks.is_empty() {
                div { "data-testid": "file-tasks-section",
                    h3 { class: "text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3",
                        "Tasks"
                    }
                    div { class: "flex flex-col",
                        for task in visible_tasks.iter() {
                            TaskItem { task: task.clone(), today: today.clone() }
                        }
                    }
                }
            }

            // Note content
            div {
                h3 { class: "text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3",
                    "Note"
                }
                match &*note_data.read() {
                    Some(Ok(data)) => rsx! {
                        div {
                            "data-testid": "note-content",
                            class: "prose prose-stone dark:prose-invert max-w-none",
                            dangerous_inner_html: "{data.html}",
                        }
                    },
                    Some(Err(e)) => rsx! {
                        p { class: "text-red-500 text-sm", "Error loading note: {e}" }
                    },
                    None => rsx! {
                        p { class: "text-sm text-stone-400 dark:text-stone-500", "Loading note…" }
                    },
                }
            }

            // Backlinks
            match &*note_data.read() {
                Some(Ok(data)) if !data.backlinks.is_empty() => rsx! {
                    div {
                        "data-testid": "backlinks-section",
                        class: "border-t border-stone-200 dark:border-stone-700 pt-4",
                        h3 { class: "text-sm font-semibold text-stone-500 dark:text-stone-400 uppercase tracking-wide mb-3",
                            "Backlinks"
                        }
                        div { class: "grid gap-1",
                            for bl_path in data.backlinks.iter() {
                                Link {
                                    to: crate::Route::VaultPath { path: urlencoding::encode(bl_path).to_string() },
                                    "data-testid": "backlink-item",
                                    class: "w-full text-left py-2 px-3 flex items-center gap-2.5 text-sm rounded-lg transition-colors hover:bg-amber-50 dark:hover:bg-amber-900/20 text-stone-600 dark:text-stone-300 hover:text-amber-600 dark:hover:text-amber-400",
                                    span { class: "text-stone-400 dark:text-stone-500", "📄" }
                                    span { class: "truncate", "{bl_path}" }
                                }
                            }
                        }
                    }
                },
                _ => rsx! {},
            }
        }
    }
}

/// Check if a task should be visible (incomplete or in-progress).
fn is_task_visible(task: &Task, _today: &str) -> bool {
    matches!(task.status, TaskStatus::Incomplete | TaskStatus::InProgress)
}

/// Count visible tasks in a subtree.
fn count_visible_tasks(node: &FolderNode, today: &str) -> usize {
    let file_count: usize = node
        .files
        .values()
        .map(|tasks| tasks.iter().filter(|t| is_task_visible(t, today)).count())
        .sum();
    let sub_count: usize = node
        .subfolders
        .values()
        .map(|sub| count_visible_tasks(sub, today))
        .sum();
    file_count + sub_count
}
