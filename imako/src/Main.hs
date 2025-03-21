module Main where

import Commonmark.Simple qualified as CM
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import Main.Utf8 qualified as Utf8
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import Text.Pandoc.Definition (Pandoc)

type Note = Either Text (Maybe Aeson.Value, Pandoc)

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    runStdoutLoggingT $ do
      baseDir <- liftIO parseArgs
      (model0, modelF) <- UM.mount baseDir (one ((), "*.md")) [] mempty (const $ handlePathUpdate baseDir)
      liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size model0) <> "; sample = " <> show (take 4 $ Map.keys model0)
      modelVar <- newTVarIO model0
      modelF $ \newModel -> do
        liftIO $ putTextLn $ "Model udpated; total docs = " <> show (Map.size newModel)
        atomically $ writeTVar modelVar newModel

handlePathUpdate :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handlePathUpdate baseDir path action = do
  case action of
    UM.Refresh _ _ -> do
      s <- decodeUtf8 <$> readFileBS (baseDir </> path)
      let doc = CM.parseMarkdownWithFrontMatter @Aeson.Value CM.fullMarkdownSpec path s
      pure $ Map.insert path doc
    UM.Delete -> do
      pure $ Map.delete path

parseArgs :: IO FilePath
parseArgs = do
  args <- getArgs
  case args of
    [path] -> pure path
    _ -> die "Usage: imako <path>"
