module Imako.Web.Static (
  getDataDirWithFallback,
  mkStaticMiddleware,
)
where

import Network.Wai (Middleware)
import Network.Wai.Middleware.Static (addBase, noDots, staticPolicy, (>->))
import Paths_imako (getDataDir)
import System.Directory (doesDirectoryExist)
import System.FilePath ((</>))

{- | Get a data directory with fallback to source tree for development
Checks both the installed location (from Cabal's data-files) and the
source tree location (for development with ghcid). Crashes with a clear
error if neither exists.
-}
getDataDirWithFallback :: String -> IO FilePath
getDataDirWithFallback subdir = do
  dataDir <- getDataDir
  let installed = dataDir </> subdir
      dev = "packages" </> "imako" </> subdir

  installedExists <- doesDirectoryExist installed
  devExists <- doesDirectoryExist dev

  pure $
    if
      | installedExists -> installed
      | devExists -> dev
      | otherwise ->
          error $
            toText $
              "Data directory not found. Tried:\n"
                <> "  - "
                <> installed
                <> "\n"
                <> "  - "
                <> dev

-- | Create static file serving middleware
mkStaticMiddleware :: IO Middleware
mkStaticMiddleware = do
  staticDir <- getDataDirWithFallback "static"
  pure $ staticPolicy (noDots >-> addBase staticDir)
