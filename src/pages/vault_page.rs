//! Main vault page — two-pane layout with sidebar and detail view.
use crate::components::folder_tree::FolderTree;
use crate::components::header::Header;
use crate::components::note_view::NoteView;
use crate::{get_folder_tree, get_note, get_vault_info, vault_events_ws};
use dioxus::prelude::*;
use dioxus_fullstack::{WebSocketOptions, use_websocket};
use ob::VaultEvent;
use std::path::PathBuf;

#[component]
pub fn VaultPage(path: Option<String>) -> Element {
  let vault_info = use_server_future(get_vault_info)?;
  let folder_tree = use_server_future(get_folder_tree)?;

  // Live note update from WebSocket: (path, note) pair
  let mut live_note = use_signal::<Option<ob::Note>>(|| None);

  // Connect to vault events WebSocket
  let mut folder_tree_res = folder_tree.clone();
  let mut vault_info_res = vault_info.clone();
  let current_path = path.as_ref().map(std::path::PathBuf::from);

  let mut socket = use_websocket(|| vault_events_ws(WebSocketOptions::new()));

  use_future(move || {
    let current_path = current_path.clone();
    async move {
      while let Ok(event) = socket.recv().await {
        match event {
          VaultEvent::NoteModified {
            path: event_path,
            note,
          } => {
            if current_path.as_ref() == Some(&event_path) {
              live_note.set(Some(note));
            }
          }
          VaultEvent::NoteAdded { .. } | VaultEvent::NoteDeleted { .. } => {
            folder_tree_res.restart();
          }
          VaultEvent::DayChanged { .. } => {
            vault_info_res.restart();
          }
        }
      }
    }
  });

  rsx! {
    div { class: "flex h-screen bg-white text-stone-800 font-sans antialiased",
      aside { class: "w-72 shrink-0 border-r border-stone-200 bg-stone-50 overflow-y-auto p-4",
        match (&*vault_info.read(), &*folder_tree.read()) {
            (Some(Ok(info)), Some(Ok(tree))) => rsx! {
              Header { info: info.clone() }
              FolderTree {
                node: tree.clone(),
                base_path: PathBuf::new(),
                selected_path: path.as_ref().map(PathBuf::from),
              }
            },
            (Some(Err(e)), _) | (_, Some(Err(e))) => rsx! {
              div { class: "text-red-600 text-sm p-4", "Error loading vault: {e}" }
            },
            _ => rsx! {
              div { class: "flex items-center justify-center p-8",
                div { class: "animate-pulse text-stone-400 text-sm", "Loading vault…" }
              }
            },
        }
      }
      main { class: "flex-1 overflow-y-auto p-8",
        match &path {
            Some(file_path) if file_path.ends_with(".md") => {
              // Use live note if it matches, otherwise fetch from server
              let file_pathbuf = PathBuf::from(file_path.as_str());
              let override_note = live_note.read().as_ref()
                .filter(|n| n.path == file_pathbuf)
                .cloned();
              if let Some(note) = override_note {
                rsx! { NoteView { note } }
              } else {
                rsx! { NoteFetcher { path: file_pathbuf } }
              }
            },
            Some(folder_path) => rsx! {
              FolderDetail {
                path: PathBuf::from(folder_path.as_str()),
                tree: folder_tree.read().as_ref().and_then(|r| r.as_ref().ok().cloned()),
              }
            },
            None => rsx! {
              RootView { tree: folder_tree.read().as_ref().and_then(|r| r.as_ref().ok().cloned()) }
            },
        }
      }
    }
  }
}

/// Fetches a note from the server and renders it.
#[component]
fn NoteFetcher(path: PathBuf) -> Element {
  let note_data = use_resource(use_reactive!(|path| async move { get_note(path).await }));
  let binding = note_data.read();
  match &*binding {
    Some(Ok(note)) => rsx! { NoteView { note: note.clone() } },
    Some(Err(e)) => {
      let msg = format!("{e}");
      rsx! {
        div { class: "text-red-600 text-sm", "Error loading note: {msg}" }
      }
    }
    None => rsx! {
      div { class: "animate-pulse text-stone-400", "Loading note…" }
    },
  }
}

/// Renders the detail view for a folder — lists its contents.
#[component]
fn FolderDetail(path: PathBuf, tree: Option<ob::FolderNode>) -> Element {
  let folder_name = path
    .file_name()
    .map(|n| n.to_string_lossy().to_string())
    .unwrap_or_else(|| path.to_string_lossy().to_string());
  rsx! {
    div { class: "space-y-6",
      h2 { class: "text-lg font-semibold text-stone-700 flex items-center gap-2",
        span { "📁" }
        span { "{folder_name}" }
      }
      if let Some(root) = &tree {
        {render_folder_contents(root, &path)}
      }
    }
  }
}

/// Navigate the tree to find and render a specific folder's contents.
fn render_folder_contents(root: &ob::FolderNode, target_path: &std::path::Path) -> Element {
  let target_str = target_path.to_string_lossy();
  let parts: Vec<&str> = target_str.split('/').collect();
  let mut current = root;
  for part in &parts {
    let mut found = false;
    for (name, node) in &current.subfolders {
      if name == *part || name.ends_with(&format!("/{}", part)) || *part == name.as_str() {
        current = node;
        found = true;
        break;
      }
    }
    if !found {
      for (name, node) in &current.subfolders {
        if target_str.starts_with(name.as_str()) || name == target_str.as_ref() {
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
    div { class: "space-y-2",
      for (name , _) in &current.subfolders {
        {
            let full_path = format!("{}/{}", target_str, name);
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
      for (filename , entry) in &current.files {
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
fn RootView(tree: Option<ob::FolderNode>) -> Element {
  rsx! {
    div { class: "space-y-6",
      h2 { class: "text-lg font-semibold text-stone-700", "Vault" }
      if let Some(root) = &tree {
        div { class: "space-y-2",
          for (name , _) in &root.subfolders {
            a {
              class: "flex items-center gap-2 py-2 px-3 rounded-lg hover:bg-stone-50 text-stone-700 cursor-pointer transition-colors",
              href: "/p/{name}",
              span { "📁" }
              span { class: "font-medium", "{name}" }
            }
          }
          for (filename , entry) in &root.files {
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
