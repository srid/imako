/**
 * AST Types for markdown rendering.
 *
 * These TypeScript types mirror the Haskell types in @ob/src/Ob/Json.hs@.
 * The duplication is intentional:
 *   - Haskell types define the source of truth for JSON serialization
 *   - TypeScript types provide static type checking on the client
 *
 * Both sides use discriminated unions with a "type" field for pattern matching.
 * Field names are 1:1 with Haskell - no hidden transformations.
 * Naming convention: `inlines` for [InlineNode], `blocks` for [BlockNode], 
 * `items` for [[BlockNode]], `text` for Text content.
 */

/**
 * Block-level nodes - structural elements like paragraphs, headings, lists.
 * Matches Haskell BlockNode in Ob.Json.
 */
export type BlockNode =
  | { type: "paragraph"; inlines: InlineNode[] }
  | { type: "heading"; level: number; inlines: InlineNode[] }
  | { type: "bulletList"; items: BlockNode[][] }
  | { type: "orderedList"; items: BlockNode[][] }
  | { type: "codeBlock"; language: string; code: string }
  | { type: "blockQuote"; blocks: BlockNode[] }
  | { type: "horizontalRule" }
  | { type: "task"; done: boolean; inlines: InlineNode[] }
  | { type: "div"; blocks: BlockNode[] }
  | { type: "rawBlock"; format: string; text: string }
  | { type: "definitionList"; definitions: unknown[] }; // TODO: proper typing

/**
 * Inline-level nodes - text formatting, links, code spans.
 * Matches Haskell InlineNode in Ob.Json.
 */
export type InlineNode =
  | { type: "text"; text: string }
  | { type: "emph"; inlines: InlineNode[] }
  | { type: "strong"; inlines: InlineNode[] }
  | { type: "underline"; inlines: InlineNode[] }
  | { type: "codeInline"; code: string }
  | { type: "link"; url: string; inlines: InlineNode[] }
  | { type: "image"; url: string; title: string; alt: InlineNode[] }
  | { type: "wikiLink"; target: string; display: string | null }
  | { type: "strikeout"; inlines: InlineNode[] }
  | { type: "superscript"; inlines: InlineNode[] }
  | { type: "subscript"; inlines: InlineNode[] }
  | { type: "smallCaps"; inlines: InlineNode[] }
  | { type: "quoted"; quoteType: string; inlines: InlineNode[] }
  | { type: "math"; mathType: string; text: string }
  | { type: "rawInline"; format: string; text: string }
  | { type: "note"; blocks: BlockNode[] }
  | { type: "span"; inlines: InlineNode[] }
  | { type: "lineBreak" };

/**
 * Top-level AST container.
 * Matches Haskell AstNode in Ob.Json.
 */
export interface AstNode {
  blocks: BlockNode[];
}
