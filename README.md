# imako

[いま](https://en.wiktionary.org/wiki/%E3%81%84%E3%81%BE) + [ここ](https://en.wiktionary.org/wiki/%E3%81%93%E3%81%93)

Imako provides journaling and planning based on the principle of [infinitude of space & time](https://srid.ca/this-moment) for your Obsidian (and [Emanote](https://emanote.srid.ca/)) notebook.

Project origin: https://x.com/sridca/status/1896560964088271161

> [!warning]
> **WIP**: The author is developing Imako as he figures out what he needs. See https://github.com/srid/imako/issues/3

## Screenshot

<img width="694" height="598" alt="image" src="https://github.com/user-attachments/assets/c097037c-9072-4d06-8d0e-f82fc417bf46" />


## Running

In Nix devShell,

```sh
# Start the backend API server
just NOTEBOOK=$HOME/mynotes run

# In another terminal, start the frontend dev server
just frontend-dev
```

Open http://localhost:5173 to view the app.

## Repository architecture

Imako is written in Haskell with a SolidJS frontend. This repository hosts:

- `packages/ob`: Haskell library for working with Obsidian vaults
- `packages/imako`: Main Imako backend (API + WebSocket server)
- `frontend/`: SolidJS SPA (Vite + Tailwind v4)
- `tests/`: Playwright E2E tests with custom DSL

## E2E Testing

```sh
# Run all tests (process-compose orchestrated)
just e2e

# Or run manually with servers already running:
just run-example    # Terminal 1
just frontend-dev   # Terminal 2
just e2e-run        # Terminal 3
```
