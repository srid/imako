# imako

[いま](https://en.wiktionary.org/wiki/%E3%81%84%E3%81%BE) + [ここ](https://en.wiktionary.org/wiki/%E3%81%93%E3%81%93)

Imako provides journaling and planning based on the principle of [infinitude of space & time](https://srid.ca/this-moment) for your Obsidian (and [Emanote](https://emanote.srid.ca/)) notebook.

> [!warning]
> **WIP**: The author is developing it as he aims to use it. See https://github.com/srid/imako/issues/3

## Running

In Nix devShell,

```sh
# Or, `just run` if you are Srid.
just NOTEBOOK=$HOME/mynotes run
```

## Repository architecture

- `packages/ob`: Haskell library for working with Obsidian vaults
- `packages/imako`: Main Imako application (uses `ob`)
