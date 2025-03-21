module Main where

import Control.Monad.Logger (runStdoutLoggingT)
import Data.Map.Strict qualified as Map
import Main.Utf8 qualified as Utf8
import System.FilePath ((</>))
import System.UnionMount qualified as UM

main :: IO ()
main = do
  Utf8.withUtf8 $ do
    runStdoutLoggingT $ do
      let baseDir = "/Users/srid/Documents/Perdurable/Text"
      (model0, modelF) <- UM.mount baseDir (one ((), "*.md")) [] mempty (const $ handlePathUpdate baseDir)
      liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size model0) <> "; sample = " <> show (take 4 $ Map.keys model0)
      modelVar <- newTVarIO model0
      modelF $ \newModel -> do
        liftIO $ putTextLn $ "Model udpated; total docs = " <> show (Map.size newModel)
        atomically $ writeTVar modelVar newModel

handlePathUpdate :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Text -> Map FilePath Text)
handlePathUpdate baseDir path action = do
  case action of
    UM.Refresh _ _ -> do
      s <- decodeUtf8 <$> readFileBS (baseDir </> path)
      pure $ Map.insert path s
    UM.Delete -> do
      pure $ Map.delete path
