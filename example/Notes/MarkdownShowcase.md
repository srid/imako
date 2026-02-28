# Markdown Showcase

This note exercises every Markdown element that Imako renders.

## Headings

### Third-Level Heading

#### Fourth-Level Heading

##### Fifth-Level Heading

###### Sixth-Level Heading

## Paragraphs

This is the first paragraph with some text to verify paragraph rendering.

This is the second paragraph. It should be visually separated from the first.

## Emphasis and Formatting

This text has **bold words** in it.

This text has *italic words* in it.

This text has ~~strikethrough~~ in it.

This has **bold and *nested italic* inside**.

This has `inline code` in a sentence.

This has [underlined text]{.underline} in it.

This has [Small Caps Text]{.smallcaps} in it.

## Lists

### Bullet List

- First bullet item
- Second bullet item
- Third bullet item with **bold**

### Nested Bullet List

- Top level
  - Nested level one
    - Nested level two
  - Another nested item

### Ordered List

1. First ordered item
2. Second ordered item
3. Third ordered item

## Blockquotes

> This is a blockquote. It should have a left border and italic styling.

> Nested blockquote:
>
> > This is a nested blockquote inside another.

## Code Blocks

### Haskell

```haskell
module Main where

import Data.Map (Map)
import qualified Data.Map as Map

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main :: IO ()
main = do
  let results = Map.fromList [(i, fib i) | i <- [0..10]]
  mapM_ (\(k, v) -> putStrLn $ show k ++ " -> " ++ show v) (Map.toList results)
```

### Rust

```rust
fn main() {
    let names = vec!["Alice", "Bob", "Charlie"];
    for name in &names {
        println!("Hello, {}!", name);
    }
}
```

### Python

```python
def greet(name: str) -> str:
    return f"Hello, {name}!"

if __name__ == "__main__":
    names = ["Alice", "Bob"]
    for name in names:
        print(greet(name))
```

### TypeScript

```typescript
interface User {
  name: string;
  age: number;
}

function greet(user: User): string {
  return `Hello, ${user.name}! You are ${user.age} years old.`;
}
```

### JavaScript

```javascript
const greet = (name) => `Hello, ${name}!`;
console.log(greet("world"));
```

### Nix

```nix
{ pkgs, lib, ... }:
{
  environment.systemPackages = with pkgs; [
    hello
    git
    vim
  ];

  services.nginx.enable = true;
}
```

### Bash

```bash
#!/bin/bash
echo "Hello, $USER!"

for file in *.txt; do
  echo "Processing: $file"
  wc -l "$file"
done
```

### YAML

```yaml
name: Build
on: [push]
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - run: make test
```

### JSON

```json
{
  "name": "imako",
  "version": "0.1.0",
  "dependencies": {
    "solid-js": "^1.8.0"
  }
}
```

### HTML

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <title>Hello</title>
</head>
<body>
  <h1>Hello, World!</h1>
</body>
</html>
```

### CSS

```css
.container {
  display: flex;
  gap: 1rem;
  color: #ff79c6;
  background: linear-gradient(135deg, #1a1a2e, #16213e);
}
```

### C

```c
#include <stdio.h>

int main() {
    printf("Hello!\n");
    return 0;
}
```

### Nested Languages (Injection)

Code inside Markdown fenced blocks — this tests whether the highlighter can parse embedded languages:

````markdown
# My Haskell Project

Here is a Fibonacci function:

```haskell
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```

And a quick shell command:

```bash
cabal run my-project -- --verbose
```
````

Nix with embedded bash (injection inside string literals):

```nix
{ pkgs, ... }:
pkgs.mkShell {
  buildInputs = [ pkgs.hello ];
  shellHook = ''
    echo "Entering dev shell..."
    export FOO="bar"
    for f in *.nix; do
      echo "Found: $f"
    done
  '';
}
```

## Horizontal Rule

---

## Tables

| Left Align | Center Align | Right Align |
|:-----------|:------------:|------------:|
| Row 1 Col 1 | Row 1 Col 2 | Row 1 Col 3 |
| Row 2 Col 1 | Row 2 Col 2 | Row 2 Col 3 |

## Definition List

Term One
:   Definition for term one.

Term Two
:   Definition for term two.

## Links

Visit [Example Website](https://example.com) for more information.

Here is an internal link: [[Welcome]].

## Superscript and Subscript

This has a superscript: H^2^O notation.

This has a subscript: H~2~O notation.

## Math

Inline math: $E = mc^2$

Display math:

$$\int_0^\infty e^{-x} dx = 1$$

## Footnotes

This sentence has a footnote[^1].

[^1]: This is the footnote content.

## Quoted Text

This has 'single quoted' text in it.

This has "double quoted" text in it.

## Line Blocks

| Line one of the block
| Line two of the block
| Line three of the block
