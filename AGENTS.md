# Agent Guidelines for Imako

## E2E Testing

**Standards:** Follow the best practices at <https://srid.ca/playwright>.

**Command:**
- `just e2e` - Run all E2E tests (starts server, runs tests, exits)

**After test runs:** Read the Playwright error-context reports in `tests/test-results/*/error-context.md` to diagnose failures. These contain page snapshots at the time of failure.

## Nix

- Untracked (not modified) files need to be git staged for Nix to recognize them.

## Git

- DO NOT FUCKING `git commit` or mutate local Git repo, ever.