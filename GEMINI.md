
# CRITICAL SETUP STEPS

- Read `ghcid.txt` for Haskell compile feeback. Remind the human to run it. Don't run `nix build`.

# UX Guidelines

UX should be simple & direct for uses of Obsidian, who are already familiar with taking notes in Markdown files, as well as the use of plugins like obsidian-tasks (tasks defined in Markdown).

## Dark Mode

The UI supports dark mode via Tailwind CSS. It automatically follows the system preference (using `prefers-color-scheme` media query). All UI components have both light and dark variants defined.

# Code Formatting

We use **fourmolu** for Haskell code formatting with the following settings:

- **indentation**: 2 spaces
- **comma-style**: leading
- **record-brace-space**: true
- **indent-wheres**: true
- **import-export-style**: diff-friendly
- **respectful**: true
- **haddock-style**: multi-line
- **newlines-between-decls**: 1
- **extensions**: `ImportQualifiedPost`

See `nix/modules/flake/pre-commit.nix` for the canonical configuration.

# Architecture

## Volatility-Based Decomposition

We follow [Volatility-based decomposition](https://www.informit.com/articles/article.aspx?p=2995357&seqNum=2) by Juval Lowy to separate concerns based on their rate of change.

### Current Layers

1. **Data Access (`ob` package)** - Low volatility
   - Reads/writes Obsidian vaults, parses Markdown
   - Provides stable API: `Vault`, `Task`, `Note`
   - Changes rarely (only when Obsidian format changes)

2. **Business Logic (`Imako.Core`)** - Medium volatility
   - Filters and groups tasks according to business rules
   - Pure transformations: `Vault` → `AppView`
   - Changes when business requirements change

3. **Presentation (`Imako.UI.*`)** - High volatility
   - HTML rendering, CSS, client-side scripts
   - Purely presentational - consumes data models and produces HTML
   - Changes frequently for UX improvements

4. **Host (`Main.hs`)** - Medium volatility
   - Wiring and coordination
   - Web server setup, HTTP handling
   - Should minimize business logic

### Encapsulation Principles

- **Hide volatility behind stable interfaces**: High-volatility code should be isolated behind APIs that change less frequently
- **Single Responsibility**: Each module should have one reason to change
- **Language boundaries OK**: Volatility can even justify language boundaries (Haskell ↔ JS) if they capture a single cohesive entity
- **Question field access**: If consumers directly access data fields (`.field`), ask whether that volatility is properly contained

### Current Limitations

We should address these:

- `AppView` is currently a DTO (Data Transfer Object), not a full encapsulation boundary
