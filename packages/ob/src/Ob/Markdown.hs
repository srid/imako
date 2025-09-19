module Ob.Markdown (
  parseMarkdown,
)
where

import Commonmark.Simple qualified as CM
import Data.Aeson qualified as Aeson
import Text.Pandoc.Definition (Pandoc)

-- | Parse markdown with front matter using the standard configuration
parseMarkdown :: FilePath -> Text -> Either Text (Maybe Aeson.Value, Pandoc)
parseMarkdown = CM.parseMarkdownWithFrontMatter @Aeson.Value CM.fullMarkdownSpec
