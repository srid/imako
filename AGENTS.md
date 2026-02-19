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

## TypeScript Types

- `frontend/src/types.ts` is AUTO-GENERATED from Haskell via `just generate-types`. NEVER edit it manually. Always run `just generate-types` after changing backend protocol types.
- After adding/removing fields in `VaultInfo`, update the `emptyVaultInfo` default in `frontend/src/store.ts`.

## Haskell

- Use `FilePath` for file/folder paths in protocol types, not `Text`. Keep types semantically consistent (e.g. `vaultPath :: FilePath`, `dailyNotesFolder :: Maybe FilePath`).

## Architecture

- Heavy lifting (parsing, data processing, structuring) belongs in the **backend**. The frontend should receive structured data and only handle rendering/UI logic. Do NOT duplicate backend logic (e.g. date parsing) in the frontend.