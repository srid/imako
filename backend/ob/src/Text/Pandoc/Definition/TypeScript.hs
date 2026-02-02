{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
TypeScript instances for Pandoc AST types.

Provides TypeScript type generation for Pandoc's Block/Inline and all
dependent types.

== JSON Format

Pandoc's native ToJSON uses short keys for efficiency:

- @"t"@ for tag (constructor name)
- @"c"@ for contents (constructor arguments)

For example, a paragraph block serializes as:

@{"t": "Para", "c": [{"t": "Str", "c": "Hello"}]}@

The generated TypeScript types match this format, using @t@ and @c@ as
discriminator and content fields respectively.

== Circular Types

Some types reference each other: Inline contains @Cite [Citation] [Inline]@,
Citation contains @[Inline]@ fields, Block contains @Note [Block]@.
These use manual TypeScript instances to break the dependency cycle.
-}
module Text.Pandoc.Definition.TypeScript (
  pandocTsDeclarations,
)
where

import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.Internal (TSDeclaration (..), TSField (..))
import Data.Aeson.TypeScript.TH (TypeScript (..), deriveTypeScript)
import Text.Pandoc.Definition hiding (Attr, ColSpec, ListAttributes, Target)

-- ===========================================================================
-- Manual TypeScript instances for circular types
-- These types reference each other: Inline contains Cite [Citation] [Inline]
--                                   Citation contains [Inline] fields
--                                   Block contains [Inline] and Note [Block]
-- ===========================================================================

instance TypeScript Inline where
  getTypeScriptType _ = "Inline"
  getTypeScriptDeclarations _ =
    [ TSTypeAlternatives
        "Inline"
        []
        [ "{ t: 'Str'; c: string }"
        , "{ t: 'Emph'; c: Inline[] }"
        , "{ t: 'Underline'; c: Inline[] }"
        , "{ t: 'Strong'; c: Inline[] }"
        , "{ t: 'Strikeout'; c: Inline[] }"
        , "{ t: 'Superscript'; c: Inline[] }"
        , "{ t: 'Subscript'; c: Inline[] }"
        , "{ t: 'SmallCaps'; c: Inline[] }"
        , "{ t: 'Quoted'; c: [QuoteType, Inline[]] }"
        , "{ t: 'Cite'; c: [Citation[], Inline[]] }"
        , "{ t: 'Code'; c: [Attr, string] }"
        , "{ t: 'Space' }"
        , "{ t: 'SoftBreak' }"
        , "{ t: 'LineBreak' }"
        , "{ t: 'Math'; c: [MathType, string] }"
        , "{ t: 'RawInline'; c: [Format, string] }"
        , "{ t: 'Link'; c: [Attr, Inline[], Target] }"
        , "{ t: 'Image'; c: [Attr, Inline[], Target] }"
        , "{ t: 'Note'; c: Block[] }"
        , "{ t: 'Span'; c: [Attr, Inline[]] }"
        ]
        Nothing
    ]

instance TypeScript Citation where
  getTypeScriptType _ = "Citation"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "Citation"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ TSField False "citationId" "string" Nothing
            , TSField False "citationPrefix" "Inline[]" Nothing
            , TSField False "citationSuffix" "Inline[]" Nothing
            , TSField False "citationMode" "CitationMode" Nothing
            , TSField False "citationNoteNum" "number" Nothing
            , TSField False "citationHash" "number" Nothing
            ]
        , interfaceDoc = Nothing
        }
    ]

instance TypeScript Block where
  getTypeScriptType _ = "Block"
  getTypeScriptDeclarations _ =
    [ TSTypeAlternatives
        "Block"
        []
        [ "{ t: 'Plain'; c: Inline[] }"
        , "{ t: 'Para'; c: Inline[] }"
        , "{ t: 'LineBlock'; c: Inline[][] }"
        , "{ t: 'CodeBlock'; c: [Attr, string] }"
        , "{ t: 'RawBlock'; c: [Format, string] }"
        , "{ t: 'BlockQuote'; c: Block[] }"
        , "{ t: 'OrderedList'; c: [ListAttributes, Block[][]] }"
        , "{ t: 'BulletList'; c: Block[][] }"
        , "{ t: 'DefinitionList'; c: [Inline[], Block[][]][] }"
        , "{ t: 'Header'; c: [number, Attr, Inline[]] }"
        , "{ t: 'HorizontalRule' }"
        , "{ t: 'Table'; c: [Attr, Caption, ColSpec[], TableHead, TableBody[], TableFoot] }"
        , "{ t: 'Figure'; c: [Attr, Caption, Block[]] }"
        , "{ t: 'Div'; c: [Attr, Block[]] }"
        ]
        Nothing
    ]

instance TypeScript Pandoc where
  getTypeScriptType _ = "Pandoc"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "Pandoc"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ TSField False "blocks" "Block[]" Nothing
            , TSField False "meta" "Meta" Nothing
            ]
        , interfaceDoc = Nothing
        }
    ]

instance TypeScript Meta where
  getTypeScriptType _ = "Meta"
  getTypeScriptDeclarations _ =
    [TSTypeAlternatives "Meta" [] ["Record<string, MetaValue>"] Nothing]

instance TypeScript MetaValue where
  getTypeScriptType _ = "MetaValue"
  getTypeScriptDeclarations _ =
    [ TSTypeAlternatives
        "MetaValue"
        []
        [ "{ tag: 'MetaMap'; contents: Record<string, MetaValue> }"
        , "{ tag: 'MetaList'; contents: MetaValue[] }"
        , "{ tag: 'MetaBool'; contents: boolean }"
        , "{ tag: 'MetaString'; contents: string }"
        , "{ tag: 'MetaInlines'; contents: Inline[] }"
        , "{ tag: 'MetaBlocks'; contents: Block[] }"
        ]
        Nothing
    ]

-- ===========================================================================
-- TH-derived TypeScript instances for non-circular types
-- ===========================================================================

$(deriveTypeScript defaultOptions ''Alignment)
$(deriveTypeScript defaultOptions ''ListNumberStyle)
$(deriveTypeScript defaultOptions ''ListNumberDelim)
$(deriveTypeScript defaultOptions ''CitationMode)
$(deriveTypeScript defaultOptions ''MathType)
$(deriveTypeScript defaultOptions ''QuoteType)
$(deriveTypeScript defaultOptions ''Format)
$(deriveTypeScript defaultOptions ''ColWidth)
$(deriveTypeScript defaultOptions ''RowSpan)
$(deriveTypeScript defaultOptions ''ColSpan)
$(deriveTypeScript defaultOptions ''RowHeadColumns)
$(deriveTypeScript defaultOptions ''Cell)
$(deriveTypeScript defaultOptions ''Row)
$(deriveTypeScript defaultOptions ''Caption)
$(deriveTypeScript defaultOptions ''TableHead)
$(deriveTypeScript defaultOptions ''TableBody)
$(deriveTypeScript defaultOptions ''TableFoot)

-- Type aliases for Pandoc
type Attr = (Text, [Text], [(Text, Text)])
type Target = (Text, Text)
type ListAttributes = (Int, ListNumberStyle, ListNumberDelim)
type ColSpec = (Alignment, ColWidth)

instance TypeScript Attr where
  getTypeScriptType _ = "Attr"
  getTypeScriptDeclarations _ =
    [TSTypeAlternatives "Attr" [] ["[string, string[], [string, string][]]"] Nothing]

instance TypeScript Target where
  getTypeScriptType _ = "Target"
  getTypeScriptDeclarations _ =
    [TSTypeAlternatives "Target" [] ["[string, string]"] Nothing]

instance TypeScript ListAttributes where
  getTypeScriptType _ = "ListAttributes"
  getTypeScriptDeclarations _ =
    [TSTypeAlternatives "ListAttributes" [] ["[number, ListNumberStyle, ListNumberDelim]"] Nothing]

instance TypeScript ColSpec where
  getTypeScriptType _ = "ColSpec"
  getTypeScriptDeclarations _ =
    [TSTypeAlternatives "ColSpec" [] ["[Alignment, ColWidth]"] Nothing]

-- ===========================================================================
-- Exports
-- ===========================================================================

-- | All Pandoc TypeScript declarations
pandocTsDeclarations :: [TSDeclaration]
pandocTsDeclarations =
  mconcat
    [ getTypeScriptDeclarations (Proxy @Attr)
    , getTypeScriptDeclarations (Proxy @Target)
    , getTypeScriptDeclarations (Proxy @ListAttributes)
    , getTypeScriptDeclarations (Proxy @ColSpec)
    , getTypeScriptDeclarations (Proxy @Alignment)
    , getTypeScriptDeclarations (Proxy @ListNumberStyle)
    , getTypeScriptDeclarations (Proxy @ListNumberDelim)
    , getTypeScriptDeclarations (Proxy @CitationMode)
    , getTypeScriptDeclarations (Proxy @MathType)
    , getTypeScriptDeclarations (Proxy @QuoteType)
    , getTypeScriptDeclarations (Proxy @Format)
    , getTypeScriptDeclarations (Proxy @ColWidth)
    , getTypeScriptDeclarations (Proxy @RowSpan)
    , getTypeScriptDeclarations (Proxy @ColSpan)
    , getTypeScriptDeclarations (Proxy @RowHeadColumns)
    , getTypeScriptDeclarations (Proxy @Cell)
    , getTypeScriptDeclarations (Proxy @Row)
    , getTypeScriptDeclarations (Proxy @Caption)
    , getTypeScriptDeclarations (Proxy @TableHead)
    , getTypeScriptDeclarations (Proxy @TableBody)
    , getTypeScriptDeclarations (Proxy @TableFoot)
    , getTypeScriptDeclarations (Proxy @Meta)
    , getTypeScriptDeclarations (Proxy @MetaValue)
    , getTypeScriptDeclarations (Proxy @Citation)
    , getTypeScriptDeclarations (Proxy @Inline)
    , getTypeScriptDeclarations (Proxy @Block)
    , getTypeScriptDeclarations (Proxy @Pandoc)
    ]
