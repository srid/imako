# Development commands for Imako

# Run the app in dev mode
run vault_path:
    dx serve --hot-reload -- --vault {{vault_path}}

# Check everything compiles
check:
    cargo check --all-targets

# Run clippy with warnings as errors
clippy:
    cargo clippy --all-targets -- -D warnings

# Run all tests
test:
    cargo test --workspace

# Format all code
fmt:
    cargo fmt --all
    nixpkgs-fmt nix/**/*.nix flake.nix

# Watch for changes and check
watch:
    bacon clippy
