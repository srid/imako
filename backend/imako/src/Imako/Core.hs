{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

{- HLINT ignore "Use infinitely" -}

module Imako.Core (
  -- * App State
  AppState (..),
  withAppState,

  -- * Message building
  mkVaultInfo,
  mkVaultData,
  mkNotesData,
  mkServerMessage,
)
where

import Commonmark.Extensions.WikiLink qualified as WikiLink
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_, withAsync)
import Data.Aeson (object, toJSON, (.=))
import Data.IxSet.Typed qualified as Ix
import Data.LVar qualified as LVar
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Time (Day, getZonedTime, localDay, zonedTimeToLocalTime)
import Imako.API.Protocol (NotesData (..), Query (..), QueryResponse (..), ServerMessage (..), VaultData (..), VaultInfo (..))
import Imako.Core.FolderTree (buildFolderTree)
import Imako.Core.FolderTree qualified as FolderTree
import Network.URI (escapeURIString, isUnreserved)
import Ob (IxNote, Note (..), Task (..), TaskStatus (..), Vault (..))
import Ob qualified
import System.FilePath (makeRelative, takeBaseName)
import Text.Pandoc.Definition (Inline (..), Pandoc)
import Text.Pandoc.Walk (walk)

-- | Application state combining vault data with runtime context
data AppState = AppState
  { vault :: Vault
  , today :: Day
  }

-- | Get today's date
getLocalToday :: IO Day
getLocalToday = localDay . zonedTimeToLocalTime <$> getZonedTime

{- | Run with a live AppState that updates on:
- Vault file changes
- Day boundary crossings
-}
withAppState :: FilePath -> (LVar.LVar AppState -> IO ()) -> IO ()
withAppState path f = do
  Ob.withLiveVault path $ \vaultVar -> do
    initialVault <- LVar.get vaultVar
    initialToday <- getLocalToday
    appStateVar <- LVar.new (AppState initialVault initialToday)

    -- Run user action alongside watchers
    withAsync (f appStateVar) $ \_ -> do
      race_
        (watchVaultChanges vaultVar appStateVar)
        (watchDayChanges appStateVar)
  where
    -- Forward vault changes to AppState
    watchVaultChanges vaultVar appStateVar = forever $ do
      newVault <- LVar.listenNext vaultVar
      LVar.modify appStateVar (\s -> s {vault = newVault})

    -- Update AppState on day boundary
    watchDayChanges appStateVar = forever $ do
      startDay <- getLocalToday
      waitForDayChange startDay
      newDay <- getLocalToday
      LVar.modify appStateVar (\s -> AppState s.vault newDay)

    waitForDayChange startDay = fix $ \loop -> do
      threadDelay (60 * 1000000) -- Check every minute
      currentDay <- getLocalToday
      when (currentDay == startDay) loop

-- | Build vault info from path and app state
mkVaultInfo :: FilePath -> AppState -> VaultInfo
mkVaultInfo path appState =
  let todayVal = appState.today
      notesMap = Map.fromList $ map (\n -> (toText n.path, n.modifiedAt)) $ Ix.toList appState.vault.notes
   in VaultInfo
        { vaultPath = path
        , vaultName = toText $ takeBaseName path
        , today = todayVal
        , notes = notesMap
        }

-- | Build vault data including all files from app state
mkVaultData :: FilePath -> AppState -> VaultData
mkVaultData vaultPath appState =
  let notes = appState.vault.notes
      allTasks = appState.vault.tasks
      enrichTask t = t {description = enrichInlines notes t.description}
      tasks = map enrichTask $ Ix.toList allTasks
      incomplete = filter (\t -> t.status /= Completed && t.status /= Cancelled) tasks
      completedTasks = filter (\t -> t.status == Completed || t.status == Cancelled) tasks
      -- Group tasks by their source note (relative path)
      groupedTasks =
        List.foldl
          ( \acc task ->
              let relativePath = makeRelative vaultPath task.sourceNote
               in Map.insertWith (flip (++)) relativePath [task] acc
          )
          Map.empty
          (incomplete <> completedTasks)
      -- Include all vault notes (those without tasks get empty lists)
      allNotePaths = map (\n -> makeRelative vaultPath n.path) $ Ix.toList notes
      allFiles = List.foldl' (\acc p -> Map.insertWith (flip (++)) p [] acc) groupedTasks allNotePaths
      tree = FolderTree.flattenTree $ buildFolderTree allFiles
   in VaultData {folderTree = tree}

-- | Build notes data by serializing the requested note to AST
mkNotesData :: FilePath -> Vault -> FilePath -> NotesData
mkNotesData _vaultPath vault reqPath =
  let ast = case Ix.getOne (Ix.getEQ reqPath vault.notes) of
        Just note -> toJSON $ enrichWikilinks vault.notes note.content
        Nothing -> toJSON (object ["error" .= ("Note not found: " <> reqPath)])
   in NotesData {notePath = reqPath, noteAst = ast}

{- | Transform wikilinks in inline elements into regular internal links.

Uses the WikiLink IxSet index for O(log n) resolution:
- Resolved: URL becomes \/n\/<encoded-path>, keeps data-wikilink for styling
- Broken: URL empty, adds data-broken attribute
-}
enrichInlines :: IxNote -> [Inline] -> [Inline]
enrichInlines notes = map enrichInline
  where
    enrichInline :: Inline -> Inline
    enrichInline inl@(Link (id', classes, kvs) linkInlines (_url, title))
      | Just (wl, _) <- WikiLink.mkWikiLinkFromInline inl =
          let resolved = Ix.getOne (Ix.getEQ wl notes)
              -- Keep data-wikilink for styling, remove data-wikilink-type
              cleanKvs = filter (\(k, _) -> k /= "data-wikilink-type") kvs
              wikilinkAttr = ("data-wikilink", _url)
           in case resolved of
                Just note ->
                  -- Resolved: set URL to internal route
                  let newUrl = "/p/" <> encodePathComponent (toText note.path)
                   in Link (id', classes, wikilinkAttr : cleanKvs) linkInlines (newUrl, title)
                Nothing ->
                  -- Broken: empty URL, add data-broken
                  let brokenAttr = ("data-broken", "true")
                   in Link (id', classes, wikilinkAttr : brokenAttr : cleanKvs) linkInlines ("", title)
    enrichInline x = x

    -- URL-encode a path component
    encodePathComponent :: Text -> Text
    encodePathComponent = toText . escapeURIString isUnreserved . toString

-- | Transform wikilinks in a Pandoc document
enrichWikilinks :: IxNote -> Pandoc -> Pandoc
enrichWikilinks notes = walk (enrichInlines notes)

-- | Build server message for a query (pure - all state in AppState)
mkServerMessage :: FilePath -> AppState -> Query -> ServerMessage
mkServerMessage vaultPath appState query =
  ServerMessage
    { vaultInfo = mkVaultInfo vaultPath appState
    , response = case query of
        VaultQuery -> VaultResponse (mkVaultData vaultPath appState)
        NotesQuery path -> NotesResponse (mkNotesData vaultPath appState.vault path)
    }
