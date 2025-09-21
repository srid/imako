
Load this into your context:
- Read `README.md`'s "Architecture" section
- To build, use `/fastci` Claude Code command. For fast build, run `cabal build` in Nix devshell.
- Coding guidelines:
  - We use `Relude` *not* the Haskell `Prelude`.
  - Use functions to de-complect logic from UI code
  - Use `flip` if and only if it makes code legible but *not* complicate it.

# UX Guidelines

Don't "design" user interfaces, visualize data structures and iterate to make it intuitive based on usage patterns.

- Expose the underlying data structure directly in the UI
- Let users see and manipulate the actual data model
- Iterate based on how users actually interact with the data
- Prioritize discoverability over aesthetic design
- Make the system's internal logic visible and accessible
