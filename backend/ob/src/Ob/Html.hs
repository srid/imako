-- | HTML rendering for Notes using Pandoc
module Ob.Html (
  noteToHtml,
)
where

import Ob.Note (Note (..))
import Text.Pandoc (
  def,
  runPure,
  writeHtml5String,
 )

-- | Render a Note's Pandoc content to HTML
noteToHtml :: Note -> Text
noteToHtml note =
  case runPure (writeHtml5String def note.content) of
    Left err -> "Error rendering HTML: " <> show err
    Right html -> html
