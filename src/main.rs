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
    <script src="https://cdn.tailwindcss.com?plugins=typography"></script>
    <style>
    /* Prose styles for rendered markdown content */
    [data-testid="note-content"] h1 { font-size: 1.875rem; font-weight: 700; margin-top: 1.5rem; margin-bottom: 0.75rem; line-height: 1.2; }
    [data-testid="note-content"] h2 { font-size: 1.5rem; font-weight: 600; margin-top: 1.5rem; margin-bottom: 0.5rem; line-height: 1.3; }
    [data-testid="note-content"] h3 { font-size: 1.25rem; font-weight: 600; margin-top: 1.25rem; margin-bottom: 0.5rem; line-height: 1.4; }
    [data-testid="note-content"] h4 { font-size: 1.125rem; font-weight: 600; margin-top: 1rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] h5 { font-size: 1rem; font-weight: 600; margin-top: 1rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] h6 { font-size: 0.875rem; font-weight: 600; margin-top: 1rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] p { margin-top: 0.5rem; margin-bottom: 0.5rem; line-height: 1.7; }
    [data-testid="note-content"] ul { list-style-type: disc; padding-left: 1.5rem; margin-top: 0.5rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] ol { list-style-type: decimal; padding-left: 1.5rem; margin-top: 0.5rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] li { margin-top: 0.25rem; margin-bottom: 0.25rem; line-height: 1.6; }
    [data-testid="note-content"] li ul, [data-testid="note-content"] li ol { margin-top: 0.25rem; margin-bottom: 0.25rem; }
    [data-testid="note-content"] pre { background-color: #1c1917; color: #e7e5e4; padding: 1rem; border-radius: 0.5rem; overflow-x: auto; margin-top: 0.75rem; margin-bottom: 0.75rem; font-size: 0.875rem; }
    [data-testid="note-content"] code { font-family: ui-monospace, SFMono-Regular, 'SF Mono', Menlo, Consolas, monospace; font-size: 0.875em; }
    [data-testid="note-content"] :not(pre) > code { background-color: #f5f5f4; padding: 0.125rem 0.375rem; border-radius: 0.25rem; color: #dc2626; }
    [data-testid="note-content"] blockquote { border-left: 4px solid #d6d3d1; padding-left: 1rem; font-style: italic; color: #78716c; margin-top: 0.75rem; margin-bottom: 0.75rem; }
    [data-testid="note-content"] blockquote blockquote { margin-top: 0.5rem; }
    [data-testid="note-content"] hr { border: none; border-top: 1px solid #d6d3d1; margin-top: 1.5rem; margin-bottom: 1.5rem; }
    [data-testid="note-content"] table { border-collapse: collapse; width: 100%; margin-top: 0.75rem; margin-bottom: 0.75rem; font-size: 0.875rem; }
    [data-testid="note-content"] th { background-color: #f5f5f4; font-weight: 600; text-align: left; padding: 0.5rem 0.75rem; border: 1px solid #d6d3d1; }
    [data-testid="note-content"] td { padding: 0.5rem 0.75rem; border: 1px solid #e7e5e4; }
    [data-testid="note-content"] dl { margin-top: 0.5rem; margin-bottom: 0.5rem; }
    [data-testid="note-content"] dt { font-weight: 600; margin-top: 0.5rem; }
    [data-testid="note-content"] dd { padding-left: 1.5rem; color: #78716c; }
    [data-testid="note-content"] a { color: #d97706; text-decoration: underline; }
    [data-testid="note-content"] a:hover { color: #b45309; }
    [data-testid="note-content"] a[data-wikilink] { color: #d97706; text-decoration: none; border-bottom: 1px dashed #d97706; }
    [data-testid="note-content"] a[data-broken] { color: #dc2626; border-bottom-color: #dc2626; }
    [data-testid="note-content"] strong { font-weight: 700; }
    [data-testid="note-content"] em { font-style: italic; }
    [data-testid="note-content"] del { text-decoration: line-through; }
    [data-testid="note-content"] sup { vertical-align: super; font-size: 0.75em; }
    [data-testid="note-content"] sub { vertical-align: sub; font-size: 0.75em; }
    [data-testid="note-content"] img { max-width: 100%; border-radius: 0.5rem; margin-top: 0.75rem; margin-bottom: 0.75rem; }
    /* Dark mode overrides */
    @media (prefers-color-scheme: dark) {
        [data-testid="note-content"] :not(pre) > code { background-color: #292524; color: #fbbf24; }
        [data-testid="note-content"] blockquote { border-left-color: #57534e; color: #a8a29e; }
        [data-testid="note-content"] hr { border-top-color: #57534e; }
        [data-testid="note-content"] th { background-color: #292524; border-color: #57534e; }
        [data-testid="note-content"] td { border-color: #44403c; }
        [data-testid="note-content"] dd { color: #a8a29e; }
        [data-testid="note-content"] a[data-broken] { color: #f87171; border-bottom-color: #f87171; }
    }
    </style>
    <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/tokyo-night-dark.min.css">
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/haskell.min.js"></script>
    <script src="https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/nix.min.js"></script>
    <script>
    // Auto-highlight code blocks after page load and hydration
    function highlightAll() { hljs.highlightAll(); }
    document.addEventListener('DOMContentLoaded', function() {
        highlightAll();
        // Re-highlight after Dioxus hydration
        const observer = new MutationObserver(function() { highlightAll(); });
        observer.observe(document.getElementById('main'), { childList: true, subtree: true });
    });
    </script>
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

    // Require vault path — either from CLI or env
    if std::env::var("IMAKO_VAULT_PATH").is_err() {
        eprintln!("Error: IMAKO_VAULT_PATH not set.");
        eprintln!("Usage: imako /path/to/vault");
        eprintln!("   or: IMAKO_VAULT_PATH=/path/to/vault imako");
        std::process::exit(1);
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
        .route("/health", axum::routing::get(|| async { "ok" }))
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
