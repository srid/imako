-- | Note parsing and data types for Obsidian notebooks
module Ob.Note (
  Note (..),
  IxNote,
  NoteIxs,
  parseNote,
)
where

import Commonmark.Extensions.WikiLink qualified as WL
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.Time (UTCTime)
import Network.URI.Slug qualified as Slug
import Ob.Markdown (parseMarkdown)
import Ob.Task (Task, extractTasks)
import System.Directory (getModificationTime)
import System.FilePath (dropExtension, splitPath, (</>))
import Text.Pandoc.Definition (Pandoc)

data Note = Note
  { path :: FilePath
  -- ^ Relative path within the vault (e.g. "folder/note.md")
  , properties :: Maybe Aeson.Value
  , content :: Pandoc
  , tasks :: [Task]
  , modifiedAt :: UTCTime
  }
  deriving stock (Eq)

-- | Notes are uniquely identified by their path within the vault.
instance Ord Note where
  compare a b = compare a.path b.path

type NoteIxs = '[FilePath, WL.WikiLink]
type IxNote = IxSet NoteIxs Note

instance Indexable NoteIxs Note where
  indices =
    ixList
      (ixFun $ one . (.path))
      (ixFun noteSelfRefs)

{- | All possible wiki-links that refer to this note.

For @Foo\/Bar\/Qux.md@ produces: @[[Qux]], [[Bar\/Qux]], [[Foo\/Bar\/Qux]]@
(with all WikiLink type variants).
-}
noteSelfRefs :: Note -> [WL.WikiLink]
noteSelfRefs note =
  let pathComponents =
        filter (not . null)
          . map (filter (/= '/'))
          $ splitPath (dropExtension note.path)
      slugs = Slug.decodeSlug . toText <$> pathComponents
   in case nonEmpty slugs of
        Nothing -> []
        Just allSlugs ->
          ordNub . toList $ snd <$> WL.allowedWikiLinks allSlugs

{- | Parse a note from disk.

@basePath@ is the vault root directory.
@relPath@ is the relative path within the vault (used as identity).
-}
parseNote :: (MonadIO m) => FilePath -> FilePath -> m Note
parseNote basePath relPath = do
  let absPath = basePath </> relPath
  s <- decodeUtf8 <$> readFileBS absPath
  mtime <- liftIO $ getModificationTime absPath
  case parseMarkdown absPath s of
    Left err -> die $ "Error parsing " <> absPath <> ": " <> show err
    Right (meta, content) -> do
      let noteTasks = extractTasks relPath content
      pure $ Note relPath meta content noteTasks mtime
