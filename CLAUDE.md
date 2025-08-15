# Claude Code Instructions for Imako

## Developer Workflow & Preferences

### Build & Testing
- Use `nix develop -c cabal build all` from project root (NOT `cabal run` without asking)
- Fix ALL warnings and hlint errors - code must be clean
- Test builds frequently during development

### Code Style & Architecture
- **Radical simplicity**: Keep code as small and simple as possible, don't go wild
- **Relude as Prelude**: Use functions exported by Relude, which is automatically imported
- **No unnecessary files**: Remove unused code
- **Minimal dependencies**: Only include what's actually used
- **Widget functions**: Create reusable UI components but keep them simple

### Haskell Conventions
- Favour relude; e.g. Use `putTextLn` instead of `putStrLn`
- Use qualified imports when functions are too generic
- Never use `cabal run` without explicit permission

### UI/UX Approach
- Google Keep-style card layouts with masonry grids
- Search-box style inputs (ultimately for filtering)
- Clean, minimal interfaces - no unnecessary buttons or complexity
- HTMX for seamless updates without page refresh

### Database Design
- Acid-state for persistence with ACID guarantees
- Simple types: Node with UUID + body text
- Edge lists over complex graph structures
- Focus on core functionality, can enrich types later
