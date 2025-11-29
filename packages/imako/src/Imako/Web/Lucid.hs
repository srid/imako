module Imako.Web.Lucid (
  AppHtml,
  liftHtml,
  runAppHtml,
  -- HTMX attributes
  hxGet_,
  hxPost_,
  hxTarget_,
  hxSwap_,
  hxSwapOob_,
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
as lazy Text suitable for HTTP responses. Newlines are stripped to ensure
compatibility with SSE (Server-Sent Events) protocol.
-}
runAppHtml :: AppView -> AppHtml () -> LT.Text
runAppHtml view html =
  let builder = runReader (execHtmlT html) view
   in LT.replace "\n" "" $ TLE.decodeUtf8 (Builder.toLazyByteString builder)

-- HTMX attribute helpers

hxGet_ :: Text -> Attributes
hxGet_ = term "hx-get"

hxPost_ :: Text -> Attributes
hxPost_ = term "hx-post"

hxTarget_ :: Text -> Attributes
hxTarget_ = term "hx-target"

hxSwap_ :: Text -> Attributes
hxSwap_ = term "hx-swap"

hxSwapOob_ :: Text -> Attributes
hxSwapOob_ = term "hx-swap-oob"
