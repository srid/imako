module Imako.UI.Lucid (
  AppHtml,
  liftHtml,
  runAppHtml,
)
where

import Data.ByteString.Builder qualified as Builder
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as TLE
import Imako.Core (AppView)
import Lucid

-- | Type alias for HTML rendering with AppView in Reader context
type AppHtml a = HtmlT (Reader AppView) a

{- | Lift pure Html into any HtmlT monad by rendering and re-parsing

This allows using third-party functions that return @Html ()@ within
our @HtmlT m ()@ context.

TODO: Find a more efficient way to do this with Lucid2 API
-}
liftHtml :: (Monad m) => Html () -> HtmlT m ()
liftHtml html = toHtmlRaw (renderText html)

{- | Run AppHtml in a specific AppView context to produce lazy Text

This extracts the HTML from the Reader monad and renders it efficiently
as lazy Text suitable for HTTP responses.
-}
runAppHtml :: AppView -> AppHtml () -> LT.Text
runAppHtml view html =
  let builder = runReader (execHtmlT html) view
   in TLE.decodeUtf8 (Builder.toLazyByteString builder)
