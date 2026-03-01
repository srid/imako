# Imako

Journaling and planning for [Obsidian](https://obsidian.md/) notebooks — rewritten in Rust with [Dioxus](https://dioxuslabs.com/).

## Features

- 📂 **Vault browsing** — Folder tree with collapsible sections
- 📝 **Note rendering** — Markdown with headings, lists, code blocks, blockquotes, emphasis
- 🔄 **Live scanning** — Vault is scanned on startup from disk
- ⚡ **Fullstack** — Single Rust binary serves both the WASM client and server API

## Getting Started

### Prerequisites

- [Nix](https://nixos.org/) with flakes enabled

### Development

```sh
# Enter the dev shell
nix develop

# Run the app (serves on http://127.0.0.1:6006)
just run

# Run with a custom vault
just run ~/my-obsidian-vault

# See all commands
just
```

### Available Commands

| Command | Description |
|---------|-------------|
| `just run [path]` | Serve the app (default: `example/` vault) |
| `just check` | Check compilation |
| `just clippy` | Run clippy with warnings as errors |
| `just test` | Run all tests |
| `just fmt` | Format Rust and Nix code |
| `just css` | Build Tailwind CSS |
| `just watch` | Watch for changes (bacon) |

## Architecture

```
imako-rust/
├── crates/ob/          # Obsidian vault library (no UI deps)
│   ├── markdown.rs     # Serializable Markdown AST
│   ├── note.rs         # Note parsing (comrak)
│   ├── vault.rs        # Vault scanning + file watching
│   └── folder_tree.rs  # Folder tree construction
├── src/                # Dioxus fullstack app
│   ├── main.rs         # Routing, server functions, app entry
│   ├── server/         # AppState, #[server] function impls
│   ├── components/     # UI components (sidebar, markdown, etc.)
│   └── pages/          # Page layouts
└── example/            # Example Obsidian vault for testing
```

## CI

```sh
nix run github:juspay/vira ci
```

## License

AGPL-3.0-or-later
