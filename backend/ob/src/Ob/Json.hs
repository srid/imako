{-# LANGUAGE DeriveAnyClass #-}

{- |
JSON AST serialization for Pandoc documents with Obsidian semantics.

These types (AstNode, BlockNode, InlineNode) define the JSON structure sent
over the websocket to the frontend. The frontend has matching TypeScript types
in @frontend/src/components/markdown/types.ts@ that mirror this structure.

The duplication is intentional: Haskell types define the source of truth for
serialization, TypeScript types provide static type checking on the client.
-}
module Ob.Json (
  noteToAst,
  AstNode (..),
  BlockNode (..),
  InlineNode (..),
)
where

import Data.Aeson (ToJSON (..), object, (.=))
import Ob.Note (Note (..))
import Text.Pandoc.Definition qualified as P

-- | Top-level AST node containing rendered markdown blocks
newtype AstNode = AstNode
  { blocks :: [BlockNode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

{- | Block-level AST nodes (paragraphs, headings, lists, etc.)
JSON uses @{type: "...", ...}@ format for frontend discriminated unions
-}
data BlockNode
  = ParagraphNode [InlineNode]
  | HeadingNode Int [InlineNode] -- level, content
  | BulletListNode [[BlockNode]]
  | OrderedListNode [[BlockNode]]
  | CodeBlockNode Text Text -- language, code
  | BlockQuoteNode [BlockNode]
  | HorizontalRuleNode
  | TaskNode Bool [InlineNode] -- done, content
  | DivNode [BlockNode]
  | RawBlockNode Text Text -- format, content
  | DefinitionListNode [([InlineNode], [[BlockNode]])]
  deriving stock (Show, Eq, Generic)

-- Custom ToJSON for discriminated union format: {type: "...", ...fields}
instance ToJSON BlockNode where
  toJSON = \case
    ParagraphNode inlines ->
      object ["type" .= ("paragraph" :: Text), "content" .= inlines]
    HeadingNode lvl inlines ->
      object ["type" .= ("heading" :: Text), "level" .= lvl, "content" .= inlines]
    BulletListNode items ->
      object ["type" .= ("bulletList" :: Text), "items" .= items]
    OrderedListNode items ->
      object ["type" .= ("orderedList" :: Text), "items" .= items]
    CodeBlockNode lang code ->
      object ["type" .= ("codeBlock" :: Text), "language" .= lang, "code" .= code]
    BlockQuoteNode blks ->
      object ["type" .= ("blockquote" :: Text), "content" .= blks]
    HorizontalRuleNode ->
      object ["type" .= ("horizontalRule" :: Text)]
    TaskNode done inlines ->
      object ["type" .= ("task" :: Text), "done" .= done, "content" .= inlines]
    DivNode blks ->
      object ["type" .= ("div" :: Text), "content" .= blks]
    RawBlockNode fmt content ->
      object ["type" .= ("rawBlock" :: Text), "format" .= fmt, "content" .= content]
    DefinitionListNode defs ->
      object ["type" .= ("definitionList" :: Text), "items" .= defs]

{- | Inline-level AST nodes (text, emphasis, links, etc.)
JSON uses @{type: "...", ...}@ format for frontend discriminated unions
-}
data InlineNode
  = TextNode Text
  | EmphNode [InlineNode]
  | StrongNode [InlineNode]
  | UnderlineNode [InlineNode]
  | CodeNode Text
  | LinkNode Text [InlineNode] -- url, content
  | ImageNode Text Text [InlineNode] -- url, title, alt
  | WikiLinkNode Text (Maybe Text) -- target, display
  | StrikeoutNode [InlineNode]
  | SuperscriptNode [InlineNode]
  | SubscriptNode [InlineNode]
  | SmallCapsNode [InlineNode]
  | QuotedNode Text [InlineNode] -- quoteType, content
  | MathNode Text Text -- mathType, content
  | RawInlineNode Text Text -- format, content
  | NoteNode [BlockNode] -- footnote content
  | SpanNode [InlineNode]
  | LineBreakNode
  deriving stock (Show, Eq, Generic)

-- Custom ToJSON for discriminated union format
instance ToJSON InlineNode where
  toJSON = \case
    TextNode t ->
      object ["type" .= ("text" :: Text), "text" .= t]
    EmphNode inlines ->
      object ["type" .= ("emphasis" :: Text), "content" .= inlines]
    StrongNode inlines ->
      object ["type" .= ("strong" :: Text), "content" .= inlines]
    UnderlineNode inlines ->
      object ["type" .= ("underline" :: Text), "content" .= inlines]
    CodeNode code ->
      object ["type" .= ("code" :: Text), "code" .= code]
    LinkNode url inlines ->
      object ["type" .= ("link" :: Text), "url" .= url, "content" .= inlines]
    ImageNode url title alt ->
      object ["type" .= ("image" :: Text), "url" .= url, "title" .= title, "alt" .= alt]
    WikiLinkNode target display ->
      object ["type" .= ("wikilink" :: Text), "target" .= target, "display" .= display]
    StrikeoutNode inlines ->
      object ["type" .= ("strikeout" :: Text), "content" .= inlines]
    SuperscriptNode inlines ->
      object ["type" .= ("superscript" :: Text), "content" .= inlines]
    SubscriptNode inlines ->
      object ["type" .= ("subscript" :: Text), "content" .= inlines]
    SmallCapsNode inlines ->
      object ["type" .= ("smallCaps" :: Text), "content" .= inlines]
    QuotedNode qtype inlines ->
      object ["type" .= ("quoted" :: Text), "quoteType" .= qtype, "content" .= inlines]
    MathNode mathType content ->
      object ["type" .= ("math" :: Text), "mathType" .= mathType, "content" .= content]
    RawInlineNode fmt content ->
      object ["type" .= ("rawInline" :: Text), "format" .= fmt, "content" .= content]
    NoteNode blks ->
      object ["type" .= ("note" :: Text), "content" .= blks]
    SpanNode inlines ->
      object ["type" .= ("span" :: Text), "content" .= inlines]
    LineBreakNode ->
      object ["type" .= ("lineBreak" :: Text)]

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
  P.RawBlock fmt rawContent -> [RawBlockNode (show fmt) rawContent]
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
  P.Code _ codeText -> [CodeNode codeText]
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
