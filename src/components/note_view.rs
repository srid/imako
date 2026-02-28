//! Note detail view — renders a file's content.

use crate::components::markdown_view::BlockRenderer;
use crate::shared::NoteData;
use dioxus::prelude::*;

#[component]
pub fn NoteView(data: NoteData) -> Element {
    let filename = data
        .note
        .path
        .file_name()
        .map(|n| n.to_string_lossy().to_string())
        .unwrap_or_else(|| "Untitled".to_string());

    rsx! {
        div {
            class: "space-y-6",
            // File header
            div {
                class: "flex items-center gap-3",
                h2 {
                    class: "text-lg font-semibold text-stone-700",
                    "{filename}"
                }
            }
            // Note content
            div {
                class: "markdown-content text-stone-700",
                BlockRenderer { blocks: data.note.content.blocks.clone() }
            }
        }
    }
}
