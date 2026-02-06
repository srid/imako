default:
    @just --list

import 'home-units/mod.just'

CABAL_REPL_ARGS:="--enable-multi-repl $MULTI_REPL_LIBRARIES"
NOTEBOOK:="$HOME/Dropbox/Vault"

# Run hoogle
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
repl *ARGS:
    cabal repl {{ CABAL_REPL_ARGS }} {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
run:
    ghcid --outputfile=ghcid.txt -T Main.main -c 'cabal repl {{ CABAL_REPL_ARGS }} imako:exe:imako' --setup ":set args {{ NOTEBOOK }}"

# Run tests
test:
    cabal test all

# Install frontend dependencies
frontend-install:
    cd frontend && npm install

# Run frontend dev server (with proxy to backend)
frontend-dev:
    cd frontend && npm run dev

# Build frontend for production
frontend-build:
    cd frontend && npm run build

# Generate TypeScript types from Haskell ToJSON instances
generate-types:
    cabal build generate-types
    nix run .#generate-types-to -- . "cabal run generate-types --"

# Run backend with example vault (for e2e tests) - uses Nix package
run-example:
    nix run .#imako -- ./example

# Install e2e test dependencies
e2e-install:
    cd tests && npm install && npx playwright install chromium

# Start dev servers (ghcid backend + Vite frontend) via process-compose
dev:
    NOTEBOOK={{ NOTEBOOK }} nix run .#dev

# Run all e2e tests (via process-compose: starts servers, runs tests, exits)
e2e:
    nix run .#e2e

# Run e2e tests (servers must already be running via `just run-example` + `just frontend-dev`)
e2e-run:
    cd tests && npm run e2e

# Run e2e tests with Playwright UI (servers must already be running via `just e2e-servers`)
e2e-ui:
    cd tests && npm run e2e:ui

# Run e2e tests with visible browser (servers must already be running)
e2e-headed:
    cd tests && npm run e2e:headed

# Run specific e2e test (by grep pattern)
e2e-quick PATTERN:
    cd tests && npx playwright test --grep "{{ PATTERN }}"
