//! Note detail view — renders a file's content.
use crate::components::markdown_view::BlockRenderer;
use dioxus::prelude::*;
use ob::Note;

#[component]
pub fn NoteView(note: Note) -> Element {
  let filename = note
    .path
    .file_name()
    .map(|n| n.to_string_lossy().to_string())
    .unwrap_or_else(|| "Untitled".to_string());
  rsx! {
    div { class: "space-y-6",
      div { class: "flex items-center gap-3",
        h2 { class: "text-lg font-semibold text-stone-700", "{filename}" }
      }
      div { class: "markdown-content text-stone-700",
        BlockRenderer { blocks: note.content.blocks.clone() }
      }
    }
  }
}
