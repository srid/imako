{- | Obsidian URL scheme for deep linking.

Provides functions to generate obsidian:// URLs for opening files in Obsidian.
See: https://help.obsidian.md/Concepts/Obsidian+URI
-}
module Ob.Link (
  obsidianOpenUrl,
) where

import Network.URI.Encode (encodeText)

{- | Generate an Obsidian URL to open a file in a vault

>>> obsidianOpenUrl "MyVault" "Notes/daily.md"
"obsidian://open?vault=MyVault&file=Notes%2Fdaily.md"
-}
obsidianOpenUrl ::
  -- | Vault name
  Text ->
  -- | Relative path to file within vault
  FilePath ->
  Text
obsidianOpenUrl vaultName relativePath =
  let encodedVault = encodeText vaultName
      encodedPath = encodeText $ toText relativePath
   in "obsidian://open?vault=" <> encodedVault <> "&file=" <> encodedPath
