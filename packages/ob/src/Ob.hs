{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note,
  Notebook,
  getNotebook,
  withLiveNotebook,
)
where

import Commonmark.Simple qualified as CM
import Control.Monad.Logger (runStdoutLoggingT)
import Data.Aeson qualified as Aeson
import Data.Map.Strict qualified as Map
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import Text.Pandoc.Definition (Pandoc)
import UnliftIO.Async (concurrently_)

type Note = Either Text (Maybe Aeson.Value, Pandoc)

type Notebook = Map FilePath Note

-- | Like `withNotebook` but returns the current snapshot, without monitoring it.
getNotebook :: FilePath -> IO Notebook
getNotebook path = do
  runStdoutLoggingT $ do
    (model0, _) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handlePathUpdate path)
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size model0) <> "; sample = " <> show (take 4 $ Map.keys model0)
    pure model0

withLiveNotebook :: FilePath -> (TVar Notebook -> IO ()) -> IO ()
withLiveNotebook path f = do
  runStdoutLoggingT $ do
    (model0, modelF) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handlePathUpdate path)
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size model0) <> "; sample = " <> show (take 4 $ Map.keys model0)
    modelVar <- newTVarIO model0
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newModel -> do
        putTextLn $ "Model udpated; total docs = " <> show (Map.size newModel)
        atomically $ writeTVar modelVar newModel

handlePathUpdate ::
  (MonadIO m) =>
  FilePath ->
  FilePath ->
  UM.FileAction () ->
  m (Map FilePath Note -> Map FilePath Note)
handlePathUpdate baseDir path action = do
  case action of
    UM.Refresh _ _ -> do
      s <- decodeUtf8 <$> readFileBS (baseDir </> path)
      let doc = CM.parseMarkdownWithFrontMatter @Aeson.Value CM.fullMarkdownSpec path s
      pure $ Map.insert path doc
    UM.Delete -> do
      pure $ Map.delete path
