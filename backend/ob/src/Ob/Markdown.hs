module Ob.Markdown (
  parseMarkdown,
)
where

import Commonmark.Extensions.WikiLink qualified as WikiLink
import Commonmark.Simple qualified as CM
import Data.Aeson qualified as Aeson
import Text.Pandoc.Definition (Pandoc)

-- | Parse markdown with front matter using the standard configuration + wikilinks
parseMarkdown :: FilePath -> Text -> Either Text (Maybe Aeson.Value, Pandoc)
parseMarkdown = CM.parseMarkdownWithFrontMatter @Aeson.Value markdownSpec
  where
    -- Full markdown spec with wikilink support
    markdownSpec = WikiLink.wikilinkSpec <> CM.fullMarkdownSpec
