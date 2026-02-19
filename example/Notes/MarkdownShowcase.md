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

```haskell
main :: IO ()
main = putStrLn "Hello, Haskell!"
```

```javascript
function greet(name) {
  return `Hello, ${name}!`;
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
