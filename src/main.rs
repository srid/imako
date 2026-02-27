#![allow(non_snake_case)]

use dioxus::prelude::*;

mod cli;
mod components;
mod server;

const MAIN_CSS: Asset = asset!("/assets/main.css");

/// Minimal HTML template for SSR (used when not built with dx build).
#[cfg(feature = "server")]
const INDEX_HTML: &str = r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Imako</title>
    <script src="https://cdn.tailwindcss.com"></script>
</head>
<body>
    <div id="main"></div>
</body>
</html>"#;

#[cfg(feature = "server")]
#[tokio::main]
async fn main() {
    use clap::Parser;
    use dioxus_fullstack::prelude::*;

    tracing_subscriber::fmt::init();

    let args = cli::Cli::parse();

    // Set vault path from CLI arg or keep existing env var
    if let Some(ref vault_path) = args.vault_path {
        let abs_path = std::fs::canonicalize(vault_path).unwrap_or_else(|_| vault_path.clone());
        std::env::set_var("IMAKO_VAULT_PATH", abs_path.to_string_lossy().as_ref());
    }

    let addr = std::net::SocketAddr::new(
        args.host
            .parse()
            .unwrap_or(std::net::IpAddr::V4(std::net::Ipv4Addr::LOCALHOST)),
        args.port,
    );

    // Build SSR config with inline index.html
    let cfg = ServeConfig::builder().index_html(INDEX_HTML.to_string());
    let serve_config: ServeConfig = cfg.try_into().expect("Failed to build ServeConfig");

    // Build the Axum router with SSR rendering + server functions
    let render_state = RenderHandleState::new(serve_config.clone(), App);
    let router = axum::Router::new()
        .register_server_functions()
        .fallback(axum::routing::get(render_handler).with_state(render_state))
        .into_make_service();

    tracing::info!("Listening on http://{}", addr);
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();
    axum::serve(listener, router).await.unwrap();
}

#[cfg(not(feature = "server"))]
fn main() {
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
