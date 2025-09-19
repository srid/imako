{- | Work with Obsidian notebooks in Haskell

WARNING: This package doesn't provide anything useful yet. See the GitHub repo for developmnt progress.
-}
module Ob (
  Note (..),
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

data Note = Note
  { properties :: Maybe Aeson.Value
  , content :: Pandoc
  }

type Notebook = Map FilePath Note

-- | Like `withNotebook` but returns the current snapshot, without monitoring it.
getNotebook :: FilePath -> IO Notebook
getNotebook path = do
  runStdoutLoggingT $ do
    (model0, _) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handleMarkdownFile path)
    liftIO $ putTextLn $ "Model ready; initial docs = " <> show (Map.size model0) <> "; sample = " <> show (take 4 $ Map.keys model0)
    pure model0

{- | Calls `f` with a `TVar` of `Notebook` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveNotebook :: FilePath -> (TVar Notebook -> IO ()) -> IO ()
withLiveNotebook path f = do
  runStdoutLoggingT $ do
    (model0, modelF) <- UM.mount path (one ((), "*.md")) [] mempty (const $ handleMarkdownFile path)
    liftIO $ putTextLn $ "Model ready; total docs = " <> show (Map.size model0)
    modelVar <- newTVarIO model0
    concurrently_ (liftIO $ f modelVar) $ do
      modelF $ \newModel -> do
        putTextLn $ "Model udpated; total docs = " <> show (Map.size newModel)
        atomically $ writeTVar modelVar newModel

handleMarkdownFile :: (MonadIO m) => FilePath -> FilePath -> UM.FileAction () -> m (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh _ _ -> do
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    pure $ Map.delete path

parseNote :: (MonadIO m) => FilePath -> m Note
parseNote path = do
  s <- decodeUtf8 <$> readFileBS path
  case CM.parseMarkdownWithFrontMatter @Aeson.Value CM.fullMarkdownSpec path s of
    Left err -> die $ show err
    Right (meta, content) -> pure $ Note meta content
