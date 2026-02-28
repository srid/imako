//! Header component — vault name and today's date.

use crate::shared::VaultInfo;
use dioxus::prelude::*;

#[component]
pub fn Header(info: VaultInfo) -> Element {
    rsx! {
        header {
            class: "flex flex-col gap-3 pb-4 mb-4 border-b border-stone-200",
            div {
                class: "flex items-center justify-between gap-4",
                h1 {
                    class: "text-lg font-bold text-stone-800",
                    "{info.vault_name}"
                }
                div {
                    class: "flex items-center gap-3",
                    span {
                        class: "text-xs text-stone-400 truncate max-w-[200px] hidden sm:block",
                        title: "{info.vault_path}",
                        "{info.vault_path}"
                    }
                    span {
                        class: "text-xs text-stone-400 ml-2 hidden sm:inline",
                        "Today: "
                        span {
                            class: "font-medium text-stone-600",
                            "{info.today}"
                        }
                    }
                }
            }
        }
    }
}
