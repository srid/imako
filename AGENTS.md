# Agent Guidelines for Imako

## E2E Testing

**Commands:**
- `just e2e` - Run all E2E tests (starts server, runs tests, exits)
- `just e2e-server` - Start server with example vault (for developing tests)
- `just e2e-test` - Run tests against running e2e-server

**Workflow for developing tests:**
1. `just e2e-server` in one terminal
2. `just e2e-test` in another terminal (fast iteration, no Nix rebuild)
