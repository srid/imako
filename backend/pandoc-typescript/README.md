# pandoc-typescript

TypeScript type generation for Pandoc AST types.

## Overview

This package provides TypeScript instances for Pandoc's AST types (`Block`, `Inline`, `Pandoc`, etc.) using [aeson-typescript](https://hackage.haskell.org/package/aeson-typescript).

## JSON Format

Pandoc's native `ToJSON` uses short keys for efficiency:
- `"t"` for tag (constructor name)
- `"c"` for contents (constructor arguments)

For example, a paragraph block serializes as:
```json
{"t": "Para", "c": [{"t": "Str", "c": "Hello"}]}
```

The generated TypeScript types match this format.

## Usage

```haskell
import Text.Pandoc.Definition.TypeScript (pandocTsDeclarations)
import Data.Aeson.TypeScript.TH (formatTSDeclarations)

main :: IO ()
main = putStrLn $ formatTSDeclarations pandocTsDeclarations
```

## Generated Types

The types are currently generated to `frontend/src/components/markdown/types.ts` via the `generate-types` executable.

**TODO:** Encapsulate the generated TypeScript file within this package (perhaps as a build artifact or embedded resource).

## Note

This package uses base (not Relude) to minimize dependencies.
