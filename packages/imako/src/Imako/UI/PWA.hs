{-# LANGUAGE DeriveAnyClass #-}

module Imako.UI.PWA (
  Manifest,
  imakoManifest,
  pwaMeta,
  serviceWorkerScript,
  serviceWorkerRegistration,
) where

import Data.Aeson (ToJSON)
import Lucid hiding (type_)

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

-- | PWA meta tags for HTML head
pwaMeta :: Html ()
pwaMeta = do
  meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
  meta_ [name_ "theme-color", content_ "#4F46E5"]
  meta_ [name_ "apple-mobile-web-app-capable", content_ "yes"]
  meta_ [name_ "apple-mobile-web-app-status-bar-style", content_ "black-translucent"]
  meta_ [name_ "apple-mobile-web-app-title", content_ "Imako"]
  link_ [rel_ "apple-touch-icon", href_ "https://fav.farm/🌌"]
  link_ [rel_ "manifest", href_ "/manifest.json"]

-- | Minimal service worker (required for PWA installation on Chrome)
serviceWorkerScript :: Text
serviceWorkerScript = "self.addEventListener('fetch', () => {});"

-- | Service worker registration
serviceWorkerRegistration :: Html ()
serviceWorkerRegistration =
  script_ "if('serviceWorker'in navigator){navigator.serviceWorker.register('/sw.js');}"
