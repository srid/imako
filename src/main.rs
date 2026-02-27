#![allow(non_snake_case)]

use dioxus::prelude::*;

mod cli;
mod components;
mod server;

const MAIN_CSS: Asset = asset!("/assets/main.css");

fn main() {
    // Parse CLI args and set environment for server functions
    #[cfg(feature = "server")]
    {
        use clap::Parser;
        let args = cli::Cli::parse();
        std::env::set_var(
            "IMAKO_VAULT_PATH",
            args.vault_path.to_string_lossy().as_ref(),
        );
    }

    dioxus::launch(App);
}

#[component]
fn App() -> Element {
    rsx! {
        document::Link { rel: "stylesheet", href: MAIN_CSS }
        Router::<Route> {}
    }
}

/// Application routes.
#[derive(Clone, Debug, PartialEq, Routable)]
enum Route {
    #[route("/")]
    VaultRoot {},
    #[route("/p/:path")]
    VaultPath { path: String },
}

#[component]
fn VaultRoot() -> Element {
    let path: Option<String> = None;
    rsx! { components::vault_page::VaultPage { selected_path: path } }
}

#[component]
fn VaultPath(path: String) -> Element {
    let decoded: String = urlencoding::decode(&path).unwrap_or_default().to_string();
    let selected: Option<String> = Some(decoded);
    rsx! { components::vault_page::VaultPage { selected_path: selected } }
}
