/**
 * AUTO-GENERATED AST types from Haskell.
 * DO NOT EDIT MANUALLY.
 *
 * Regenerate with: just generate-types
 *
 * Source: packages/ob/src/Text/Pandoc/Definition/TypeScript.hs
 *
 * These types match Pandoc's Block/Inline AST for markdown rendering.
 * Uses discriminated unions with "tag" field for pattern matching.
 */

export type Attr = [string, string[], [string, string][]];

export type Target = [string, string];

export type ListAttributes = [number, ListNumberStyle, ListNumberDelim];

export type ColSpec = [Alignment, ColWidth];

// Alignment of a table column.
export type Alignment = "AlignLeft" | "AlignRight" | "AlignCenter" | "AlignDefault";

// Style of list numbers.
export type ListNumberStyle = "DefaultStyle" | "Example" | "Decimal" | "LowerRoman" | "UpperRoman" | "LowerAlpha" | "UpperAlpha";

// Delimiter of list numbers.
export type ListNumberDelim = "DefaultDelim" | "Period" | "OneParen" | "TwoParens";

export type CitationMode = "AuthorInText" | "SuppressAuthor" | "NormalCitation";

// Type of math element (display or inline).
export type MathType = "DisplayMath" | "InlineMath";

// Type of quotation marks to use in Quoted inline.
export type QuoteType = "SingleQuote" | "DoubleQuote";

// Formats for raw blocks
export type Format = IFormat;

export type IFormat = string;

// The width of a table column, as a percentage of the text width.
export type ColWidth = IColWidth | IColWidthDefault;

export interface IColWidth {
  tag: "ColWidth";
  contents: number;
}

export interface IColWidthDefault {
  tag: "ColWidthDefault";
}

// The number of rows occupied by a cell; the height of a cell.
export type RowSpan = IRowSpan;

export type IRowSpan = number;

// The number of columns occupied by a cell; the width of a cell.
export type ColSpan = IColSpan;

export type IColSpan = number;

// The number of columns taken up by the row head of each row of a
// 'TableBody'. The row body takes up the remaining columns.
export type RowHeadColumns = IRowHeadColumns;

export type IRowHeadColumns = number;

// A table cell.
export type Cell = ICell;

export type ICell = [[string, string[], [string, string][]], Alignment, RowSpan, ColSpan, Block[]];

// A table row.
export type Row = IRow;

export type IRow = [[string, string[], [string, string][]], Cell[]];

// The caption of a table or figure, with optional short caption.
export type Caption = ICaption;

export type ICaption = [Inline[] | null, Block[]];

// The head of a table.
export type TableHead = ITableHead;

export type ITableHead = [[string, string[], [string, string][]], Row[]];

// A body of a table, with an intermediate head, intermediate body,
// and the specified number of row header columns in the intermediate
// body.
export type TableBody = ITableBody;

export type ITableBody = [[string, string[], [string, string][]], RowHeadColumns, Row[], Row[]];

// The foot of a table.
export type TableFoot = ITableFoot;

export type ITableFoot = [Attr, Row[]];

export type Meta = Record<string, MetaValue>;

export type MetaValue = { tag: 'MetaMap'; contents: Record<string, MetaValue> } | { tag: 'MetaList'; contents: MetaValue[] } | { tag: 'MetaBool'; contents: boolean } | { tag: 'MetaString'; contents: string } | { tag: 'MetaInlines'; contents: Inline[] } | { tag: 'MetaBlocks'; contents: Block[] };

export interface Citation {
  citationId: string;
  citationPrefix: Inline[];
  citationSuffix: Inline[];
  citationMode: CitationMode;
  citationNoteNum: number;
  citationHash: number;
}

export type Inline = { t: 'Str'; c: string } | { t: 'Emph'; c: Inline[] } | { t: 'Underline'; c: Inline[] } | { t: 'Strong'; c: Inline[] } | { t: 'Strikeout'; c: Inline[] } | { t: 'Superscript'; c: Inline[] } | { t: 'Subscript'; c: Inline[] } | { t: 'SmallCaps'; c: Inline[] } | { t: 'Quoted'; c: [QuoteType, Inline[]] } | { t: 'Cite'; c: [Citation[], Inline[]] } | { t: 'Code'; c: [Attr, string] } | { t: 'Space' } | { t: 'SoftBreak' } | { t: 'LineBreak' } | { t: 'Math'; c: [MathType, string] } | { t: 'RawInline'; c: [Format, string] } | { t: 'Link'; c: [Attr, Inline[], Target] } | { t: 'Image'; c: [Attr, Inline[], Target] } | { t: 'Note'; c: Block[] } | { t: 'Span'; c: [Attr, Inline[]] };

export type Block = { t: 'Plain'; c: Inline[] } | { t: 'Para'; c: Inline[] } | { t: 'LineBlock'; c: Inline[][] } | { t: 'CodeBlock'; c: [Attr, string] } | { t: 'RawBlock'; c: [Format, string] } | { t: 'BlockQuote'; c: Block[] } | { t: 'OrderedList'; c: [ListAttributes, Block[][]] } | { t: 'BulletList'; c: Block[][] } | { t: 'DefinitionList'; c: [Inline[], Block[][]][] } | { t: 'Header'; c: [number, Attr, Inline[]] } | { t: 'HorizontalRule' } | { t: 'Table'; c: [Attr, Caption, ColSpec[], TableHead, TableBody[], TableFoot] } | { t: 'Figure'; c: [Attr, Caption, Block[]] } | { t: 'Div'; c: [Attr, Block[]] };

export interface Pandoc {
  blocks: Block[];
  meta: Meta;
}
