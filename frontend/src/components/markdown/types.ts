/**
 * AST Types for markdown rendering.
 *
 * These TypeScript types mirror the Haskell types in @ob/src/Ob/Json.hs@.
 * The duplication is intentional:
 *   - Haskell types define the source of truth for JSON serialization
 *   - TypeScript types provide static type checking on the client
 *
 * Both sides use discriminated unions with a "type" field for pattern matching.
 */

/**
 * Block-level nodes - structural elements like paragraphs, headings, lists.
 * Matches Haskell BlockNode in Ob.Json.
 */
export type BlockNode =
  | { type: "paragraph"; content: InlineNode[] }
  | { type: "heading"; level: number; content: InlineNode[] }
  | { type: "bulletList"; items: BlockNode[][] }
  | { type: "orderedList"; items: BlockNode[][] }
  | { type: "codeBlock"; language: string; code: string }
  | { type: "blockquote"; content: BlockNode[] }
  | { type: "horizontalRule" }
  | { type: "task"; done: boolean; content: InlineNode[] }
  | { type: "div"; content: BlockNode[] }
  | { type: "rawBlock"; format: string; content: string }
  | { type: "definitionList"; items: unknown[] }; // TODO: proper typing

/**
 * Inline-level nodes - text formatting, links, code spans.
 * Matches Haskell InlineNode in Ob.Json.
 */
export type InlineNode =
  | { type: "text"; text: string }
  | { type: "emphasis"; content: InlineNode[] }
  | { type: "strong"; content: InlineNode[] }
  | { type: "underline"; content: InlineNode[] }
  | { type: "code"; code: string }
  | { type: "link"; url: string; content: InlineNode[] }
  | { type: "image"; url: string; title: string; alt: InlineNode[] }
  | { type: "wikilink"; target: string; display: string | null }
  | { type: "strikeout"; content: InlineNode[] }
  | { type: "superscript"; content: InlineNode[] }
  | { type: "subscript"; content: InlineNode[] }
  | { type: "smallCaps"; content: InlineNode[] }
  | { type: "quoted"; quoteType: string; content: InlineNode[] }
  | { type: "math"; mathType: string; content: string }
  | { type: "rawInline"; format: string; content: string }
  | { type: "note"; content: BlockNode[] }
  | { type: "span"; content: InlineNode[] }
  | { type: "lineBreak" };

/**
 * Top-level AST container.
 * Matches Haskell AstNode in Ob.Json.
 */
export interface AstNode {
  blocks: BlockNode[];
}
