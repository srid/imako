{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- |
JSON AST serialization for Pandoc documents with Obsidian semantics.

== Why separate AST types?

We don't reuse Pandoc's AST directly because:

1. We add Obsidian-specific nodes ('TaskNode', 'WikiLinkNode')
2. We produce a clean discriminated union format for TypeScript

== Why TypeScript mirrors these types?

The frontend (@frontend/src/components/markdown/types.ts@) defines matching
types. Haskell is source of truth, TypeScript provides client type checking.

== JSON Format

Uses @genericToJSON@ with 'TaggedObject' encoding to produce:

@
{"type": "paragraph", "content": [...]}
{"type": "heading", "level": 2, "content": [...]}
@

This format works with TypeScript discriminated unions.

Uses DuplicateRecordFields + fieldLabelModifier to normalize field names
for the frontend (e.g., "inlineContent" -> "content").
-}
module Ob.Json (
  noteToAst,
  AstNode (..),
  BlockNode (..),
  InlineNode (..),
)
where

import Data.Aeson (Options (..), SumEncoding (..), ToJSON (..), defaultOptions, genericToJSON)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Ob.Note (Note (..))
import Text.Pandoc.Definition qualified as P

-- | JSON options for discriminated union format
astOptions :: Options
astOptions =
  defaultOptions
    { sumEncoding = TaggedObject "type" "contents"
    , constructorTagModifier = lcFirst . dropNodeSuffix
    , allNullaryToStringTag = False
    , tagSingleConstructors = True
    }
  where
    dropNodeSuffix s = fromMaybe s (stripSuffix "Node" s)
    stripSuffix suffix s = reverse <$> stripPrefix (reverse suffix) (reverse s)
    lcFirst [] = []
    lcFirst (c : cs) = toLower c : cs

-- | Top-level AST node containing rendered markdown blocks
newtype AstNode = AstNode
  { blocks :: [BlockNode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

{- | Block-level AST nodes (paragraphs, headings, lists, etc.)
Uses DuplicateRecordFields: `inlines` for [InlineNode], `blocks` for [BlockNode], `items` for [[BlockNode]].
-}
data BlockNode
  = ParagraphNode {inlines :: [InlineNode]}
  | HeadingNode {level :: Int, inlines :: [InlineNode]}
  | BulletListNode {items :: [[BlockNode]]}
  | OrderedListNode {items :: [[BlockNode]]}
  | CodeBlockNode {language :: Text, code :: Text}
  | BlockQuoteNode {blocks :: [BlockNode]}
  | HorizontalRuleNode
  | TaskNode {done :: Bool, inlines :: [InlineNode]}
  | DivNode {blocks :: [BlockNode]}
  | RawBlockNode {format :: Text, text :: Text}
  | DefinitionListNode {definitions :: [([InlineNode], [[BlockNode]])]}
  deriving stock (Show, Eq, Generic)

instance ToJSON BlockNode where
  toJSON = genericToJSON astOptions

{- | Inline-level AST nodes (text, emphasis, links, etc.)
Uses DuplicateRecordFields: `inlines` for [InlineNode], `blocks` for [BlockNode].
-}
data InlineNode
  = TextNode {text :: Text}
  | EmphNode {inlines :: [InlineNode]}
  | StrongNode {inlines :: [InlineNode]}
  | UnderlineNode {inlines :: [InlineNode]}
  | CodeInlineNode {code :: Text}
  | LinkNode {url :: Text, inlines :: [InlineNode]}
  | ImageNode {url :: Text, title :: Text, alt :: [InlineNode]}
  | WikiLinkNode {target :: Text, display :: Maybe Text}
  | StrikeoutNode {inlines :: [InlineNode]}
  | SuperscriptNode {inlines :: [InlineNode]}
  | SubscriptNode {inlines :: [InlineNode]}
  | SmallCapsNode {inlines :: [InlineNode]}
  | QuotedNode {quoteType :: Text, inlines :: [InlineNode]}
  | MathNode {mathType :: Text, text :: Text}
  | RawInlineNode {format :: Text, text :: Text}
  | NoteNode {blocks :: [BlockNode]}
  | SpanNode {inlines :: [InlineNode]}
  | LineBreakNode
  deriving stock (Show, Eq, Generic)

instance ToJSON InlineNode where
  toJSON = genericToJSON astOptions

-- | Convert a Note to JSON AST
noteToAst :: Note -> AstNode
noteToAst note =
  let P.Pandoc _ pandocBlocks = note.content
   in AstNode {blocks = concatMap convertBlock pandocBlocks}

-- | Convert Pandoc Block to BlockNodes
convertBlock :: P.Block -> [BlockNode]
convertBlock = \case
  P.Plain inlines -> convertPlainOrTask inlines
  P.Para inlines -> [convertParaOrTask inlines]
  P.Header lvl _ inlines -> [HeadingNode lvl (convertInlines inlines)]
  P.BulletList listItems -> convertListItems listItems
  P.OrderedList _ listItems -> convertListItems listItems
  P.CodeBlock (_, classes, _) codeText ->
    [CodeBlockNode (fromMaybe "" $ viaNonEmpty head classes) codeText]
  P.BlockQuote blks -> [BlockQuoteNode (concatMap convertBlock blks)]
  P.HorizontalRule -> [HorizontalRuleNode]
  P.RawBlock fmt rawCont -> [RawBlockNode (show fmt) rawCont]
  P.DefinitionList defs ->
    [DefinitionListNode [(convertInlines t, map (concatMap convertBlock) ds) | (t, ds) <- defs]]
  P.Div _ blks -> [DivNode (concatMap convertBlock blks)]
  P.Table {} -> [] -- Tables not yet supported
  P.Figure _ _ blks -> [DivNode (concatMap convertBlock blks)]
  P.LineBlock lineGroups -> map (ParagraphNode . convertInlines) lineGroups

-- | Convert Plain block - check if it's an Obsidian task
convertPlainOrTask :: [P.Inline] -> [BlockNode]
convertPlainOrTask inlines =
  case convertParaOrTask inlines of
    ParagraphNode [] -> []
    node -> [node]

-- | Convert list items - flatten when items contain single tasks
convertListItems :: [[P.Block]] -> [BlockNode]
convertListItems listItems =
  let converted = map convertListItem listItems
   in [BulletListNode converted | not (all null converted)]

-- | Convert a single list item
convertListItem :: [P.Block] -> [BlockNode]
convertListItem = \case
  [P.Plain inlines] -> convertPlainOrTask inlines
  [P.Para inlines] ->
    case convertParaOrTask inlines of
      ParagraphNode [] -> []
      node -> [node]
  blks -> concatMap convertBlock blks

-- | Convert paragraph, detecting Obsidian tasks marked with ☐/☑/☒ characters
convertParaOrTask :: [P.Inline] -> BlockNode
convertParaOrTask = \case
  P.Str marker : P.Space : rest
    | marker `elem` ["☐", "☑", "☒"] -> TaskNode (marker /= "☐") (convertInlines rest)
  P.Str marker : rest
    | marker `elem` ["☐", "☑", "☒"] -> TaskNode (marker /= "☐") (convertInlines rest)
  inlines -> ParagraphNode (convertInlines inlines)

-- | Convert Pandoc Inlines to InlineNodes - handles all Pandoc inline types
convertInlines :: [P.Inline] -> [InlineNode]
convertInlines = concatMap convertInline

convertInline :: P.Inline -> [InlineNode]
convertInline = \case
  P.Str s -> [TextNode s]
  P.Space -> [TextNode " "]
  P.SoftBreak -> [TextNode " "]
  P.LineBreak -> [LineBreakNode]
  P.Emph inlines -> [EmphNode (convertInlines inlines)]
  P.Underline inlines -> [UnderlineNode (convertInlines inlines)]
  P.Strong inlines -> [StrongNode (convertInlines inlines)]
  P.Strikeout inlines -> [StrikeoutNode (convertInlines inlines)]
  P.Superscript inlines -> [SuperscriptNode (convertInlines inlines)]
  P.Subscript inlines -> [SubscriptNode (convertInlines inlines)]
  P.SmallCaps inlines -> [SmallCapsNode (convertInlines inlines)]
  P.Code _ codeText -> [CodeInlineNode codeText]
  P.Link _ inlines (linkUrl, _) -> [LinkNode linkUrl (convertInlines inlines)]
  P.Image _ altInlines (imgUrl, imgTitle) -> [ImageNode imgUrl imgTitle (convertInlines altInlines)]
  P.Span (_, ["wikilink"], _) [P.Str wlTarget] -> [WikiLinkNode wlTarget Nothing]
  P.Span (_, ["wikilink"], _) inlines -> [WikiLinkNode (extractText inlines) Nothing]
  P.Span _ inlines -> [SpanNode (convertInlines inlines)]
  P.Quoted P.SingleQuote inlines -> [QuotedNode "single" (convertInlines inlines)]
  P.Quoted P.DoubleQuote inlines -> [QuotedNode "double" (convertInlines inlines)]
  P.Math mt mathText -> [MathNode (show mt) mathText]
  P.RawInline fmt rawText -> [RawInlineNode (show fmt) rawText]
  P.Note blks -> [NoteNode (concatMap convertBlock blks)]
  P.Cite _ inlines -> convertInlines inlines

-- | Extract plain text from inlines (for wikilink targets)
extractText :: [P.Inline] -> Text
extractText = mconcat . map inlineToText
  where
    inlineToText :: P.Inline -> Text
    inlineToText = \case
      P.Str s -> s
      P.Space -> " "
      _ -> ""
