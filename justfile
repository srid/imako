# Development commands for Imako
default:
    @just --list

# Run the app with the example vault
run vault_path="example":
    VAULT_PATH={{vault_path}} dx serve

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
