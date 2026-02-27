default:
    @just --list

NOTEBOOK:=justfile_directory() / "example"

# Run the Dioxus dev server
[group('dev')]
dev:
    IMAKO_VAULT_PATH={{ NOTEBOOK }} dx serve

# Run tests
[group('dev')]
test:
    cargo test --workspace

# Run clippy
[group('dev')]
clippy:
    cargo clippy --workspace -- -D warnings

# Format code
[group('dev')]
fmt:
    cargo fmt --all

# Check formatting
[group('dev')]
fmt-check:
    cargo fmt --all -- --check

# Install e2e test dependencies
[group('e2e')]
e2e-install:
    cd tests && npm install && npx playwright install chromium

# Start dev servers via process-compose
[group('dev')]
dev-compose:
    NOTEBOOK={{ NOTEBOOK }} nix run .#dev

# Run all e2e tests
[group('e2e')]
e2e:
    nix run .#e2e
