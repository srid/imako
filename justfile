# Development commands for Imako

# Run the app with the example vault
run vault_path="example":
    cargo run -- --vault {{vault_path}}

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
