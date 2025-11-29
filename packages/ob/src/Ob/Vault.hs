-- | Vault data types and operations for Obsidian notebooks
module Ob.Vault (
  Vault (..),
  getTasks,
  getVault,
  withLiveVault,
)
where

import Effectful (Eff, IOE, withEffToIO, (:>))
import Effectful.Colog (Log)
import Effectful.Colog.Simple (LogContext, Severity (..), log, withLogContext)
import Effectful.Internal.Monad (Limit (Unlimited), Persistence (Ephemeral), UnliftStrategy (ConcUnlift))
import Effectful.Reader.Static qualified as ER

import Colog (LogAction (..), WithSeverity (..))
import Colog.Message (RichMessage)
import Data.LVar (LVar)
import Data.LVar qualified as LVar
import Data.Map.Strict qualified as Map
import Ob.Note (Note (..), parseNote)
import Ob.Task (Task)
import System.FilePath ((</>))
import System.UnionMount qualified as UM
import UnliftIO.Async (concurrently_)

-- | An Obsidian vault folder
newtype Vault = Vault
  { notes :: Map.Map FilePath Note
  }

getTasks :: Vault -> [Task]
getTasks vault = concatMap tasks (Map.elems vault.notes)

{- | Like `withVault` but returns the current snapshot, without monitoring it.
| Like `withVault` but returns the current snapshot, without monitoring it.
-}
getVault :: (ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => FilePath -> Eff es Vault
getVault path = do
  Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
    (notesMap, _) <- mountVault path
    log Info $ "Model ready; initial docs = " <> show (Map.size notesMap) <> "; sample = " <> show (take 4 $ Map.keys notesMap)
    pure $ Vault notesMap

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}

{- | Calls `f` with a `LVar` of `Vault` reflecting its current state in real-time.

Uses `System.UnionMount` to monitor the filesystem for changes.
-}
withLiveVault :: (ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => FilePath -> (LVar Vault -> IO ()) -> Eff es ()
withLiveVault path f = do
  Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
    (notesMap0, modelF) <- mountVault path
    let initialVault = Vault notesMap0
    log Info $ "Model ready; total docs = " <> show (Map.size notesMap0)
    modelVar <- LVar.new initialVault
    withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO -> do
      liftIO $ concurrently_ (f modelVar) $ do
        modelF $ \newNotesMap -> runInIO $ do
          Effectful.Colog.Simple.withLogContext [("context", "Vault")] $ do
            let newVault = Vault newNotesMap
            let newTasks = getTasks newVault
            log Info $ "Model updated; total docs = " <> show (Map.size newNotesMap) <> "; total tasks = " <> show (length newTasks)
            liftIO $ LVar.set modelVar newVault

mountVault ::
  (ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) =>
  FilePath ->
  Eff
    es
    ( Map FilePath Note
    , (Map FilePath Note -> IO ()) -> IO ()
    )
mountVault path = withEffToIO (ConcUnlift Ephemeral Unlimited) $ \runInIO -> do
  let logger = LogAction $ \(WithSeverity msg sev) -> runInIO $ log sev msg
  (initial, umCallback) <- UM.mount logger path (one ((), "**/*.md")) ["**/.*/**"] mempty (\_ fp act -> runInIO $ handleMarkdownFile path fp act)
  let finalCallback userAction = do
        umCallback $ \notes -> liftIO $ userAction notes
  pure (initial, finalCallback)

handleMarkdownFile :: (ER.Reader LogContext :> es, Log (RichMessage IO) :> es, IOE :> es) => FilePath -> FilePath -> UM.FileAction () -> Eff es (Map FilePath Note -> Map FilePath Note)
handleMarkdownFile baseDir path = \case
  UM.Refresh rAction _ -> do
    unless (rAction == UM.Existing) $ log Info $ "Refreshing [" <> show rAction <> "] " <> toText path
    note <- parseNote $ baseDir </> path
    pure $ Map.insert path note
  UM.Delete -> do
    log Info $ "Deleting " <> toText path
    pure $ Map.delete path
