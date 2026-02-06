default:
    @just --list

import 'home-units/mod.just'

CABAL_REPL_ARGS:="--enable-multi-repl $MULTI_REPL_LIBRARIES"
NOTEBOOK:="$HOME/Dropbox/Vault"

# Run hoogle
[group('dev')]
docs:
    echo http://127.0.0.1:8888
    hoogle serve -p 8888 --local

# Run cabal repl
[group('dev')]
repl *ARGS:
    cabal repl {{ CABAL_REPL_ARGS }} {{ ARGS }}

# Run ghcid -- auto-recompile and run `main` function
[group('backend')]
backend-dev:
    ghcid --outputfile=ghcid.txt -T Main.main -c 'cabal repl {{ CABAL_REPL_ARGS }} imako:exe:imako' --setup ":set args {{ NOTEBOOK }}"

# Run tests
[group('backend')]
test:
    cabal test all

# Install frontend dependencies
[group('frontend')]
frontend-install:
    cd frontend && npm install

# Run frontend dev server (with proxy to backend)
[group('frontend')]
frontend-dev:
    cd frontend && npm run dev

# Build frontend for production
[group('frontend')]
frontend-build:
    cd frontend && npm run build

# Generate TypeScript types from Haskell ToJSON instances
[group('types')]
generate-types:
    cabal build generate-types
    nix run .#generate-types-to -- . "cabal run generate-types --"

# Install e2e test dependencies
[group('e2e')]
e2e-install:
    cd tests && npm install && npx playwright install chromium

# Start dev servers (ghcid backend + Vite frontend) via process-compose
[group('dev')]
dev:
    NOTEBOOK={{ NOTEBOOK }} nix run .#dev

# Run all e2e tests (via process-compose: starts servers, runs tests, exits)
[group('e2e')]
e2e:
    nix run .#e2e

# Run e2e tests (servers must already be running via `just dev`)
[group('e2e')]
e2e-run:
    cd tests && npm run e2e

# Run e2e tests with Playwright UI (servers must already be running via `just dev`)
[group('e2e')]
e2e-ui:
    cd tests && npm run e2e:ui

# Run e2e tests with visible browser (servers must already be running)
[group('e2e')]
e2e-headed:
    cd tests && npm run e2e:headed

# Run specific e2e test (by grep pattern)
[group('e2e')]
e2e-quick PATTERN:
    cd tests && npx playwright test --grep "{{ PATTERN }}"
