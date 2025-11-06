{-# LANGUAGE DeriveAnyClass #-}

module Imako.UI.PWA (
  Manifest,
  imakoManifest,
) where

import Data.Aeson (ToJSON)

-- | PWA manifest icon
data ManifestIcon = ManifestIcon
  { src :: Text
  , sizes :: Text
  , type_ :: Text
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

-- | PWA manifest
data Manifest = Manifest
  { name :: Text
  , short_name :: Text
  , description :: Text
  , start_url :: Text
  , display :: Text
  , background_color :: Text
  , theme_color :: Text
  , icons :: [ManifestIcon]
  }
  deriving stock (Generic, Show)
  deriving anyclass (ToJSON)

-- | Default Imako PWA manifest
imakoManifest :: Manifest
imakoManifest =
  Manifest
    { name = "Imako"
    , short_name = "Imako"
    , description = "Journaling and planning for Obsidian"
    , start_url = "/"
    , display = "standalone"
    , background_color = "#1F2937"
    , theme_color = "#4F46E5"
    , icons = [ManifestIcon {src = "https://fav.farm/🌌", sizes = "any", type_ = "image/svg+xml"}]
    }
