//! Header component — vault name and today's date.
use crate::shared::VaultInfo;
use dioxus::prelude::*;

#[component]
pub fn Header(info: VaultInfo) -> Element {
  let vault_path = info.vault_path.display().to_string();
  rsx! {
    header { class: "pb-4 mb-4 border-b border-stone-200",
      div { class: "flex items-center justify-between gap-4",
        h1 {
          class: "text-lg font-bold text-stone-800",
          title: "{vault_path}",
          "{info.vault_name}"
        }
        span { class: "text-xs font-medium text-stone-600", "{info.today}" }
      }
    }
  }
}
