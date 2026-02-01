{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- |
Generate TypeScript types from Haskell ToJSON instances.

Usage:
  cabal run generate-types > frontend/src/types.ts
  -- or --
  just generate-types

This ensures type-safety between the Haskell backend and TypeScript frontend.
-}
module Main where

import Data.Aeson (defaultOptions)
import Data.Aeson.TypeScript.Internal (TSDeclaration (..), TSField (..))
import Data.Aeson.TypeScript.TH
import Data.List qualified
import Data.Time (Day)
import Imako.Core (AppView (..))
import Imako.Core.Filter (Filter (..))
import Imako.Core.FolderTree (FolderNode (..))
import Ob.DailyNotes (DailyNote (..))
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Task.Properties (Priority (..))
import Text.Pandoc.Definition (Pandoc)

-- Derive TypeScript instances for types with generic ToJSON
$(deriveTypeScript defaultOptions ''TaskStatus)
$(deriveTypeScript defaultOptions ''Priority)
$(deriveTypeScript defaultOptions ''Filter)

-- Day is serialized as a string (ISO date)
instance TypeScript Day where
  getTypeScriptType _ = "string"
  getTypeScriptDeclarations _ = [] -- No separate declaration needed

-- Pandoc isn't used in JSON output, but we need an instance for DailyNote
instance TypeScript Pandoc where
  getTypeScriptType _ = "never"
  getTypeScriptDeclarations _ = []

-- Task has a custom ToJSON, so we need to manually define what it looks like
-- This matches the JSON output from Task.hs ToJSON instance
instance TypeScript Task where
  getTypeScriptType _ = "Task"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "Task"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ TSField False "description" "string" Nothing
            , TSField False "sourceNote" "string" Nothing
            , TSField False "status" "TaskStatus" Nothing
            , TSField True "dueDate" "string" Nothing
            , TSField True "scheduledDate" "string" Nothing
            , TSField True "startDate" "string" Nothing
            , TSField True "completedDate" "string" Nothing
            , TSField False "priority" "Priority" Nothing
            , TSField False "tags" "string[]" Nothing
            , TSField False "parentBreadcrumbs" "string[]" Nothing
            ]
        , interfaceDoc = Nothing
        }
    ]

-- DailyNote has a custom ToJSON that only outputs day and notePath
instance TypeScript DailyNote where
  getTypeScriptType _ = "DailyNote"
  getTypeScriptDeclarations _ =
    [ TSInterfaceDeclaration
        { interfaceName = "DailyNote"
        , interfaceGenericVariables = []
        , interfaceMembers =
            [ TSField False "day" "string" Nothing -- Day is serialized as ISO string
            , TSField False "notePath" "string" Nothing
            ]
        , interfaceDoc = Nothing
        }
    ]

-- Now we can derive FolderNode and AppView since Task and DailyNote have TypeScript instances
$(deriveTypeScript defaultOptions ''FolderNode)
$(deriveTypeScript defaultOptions ''AppView)

main :: IO ()
main = do
  putStrLn header
  putStrLn $ formatTSDeclarations' defaultFormattingOptions allDecls
  where
    allDecls =
      mconcat
        [ getTypeScriptDeclarations (Proxy @TaskStatus)
        , getTypeScriptDeclarations (Proxy @Priority)
        , getTypeScriptDeclarations (Proxy @Task)
        , getTypeScriptDeclarations (Proxy @Filter)
        , getTypeScriptDeclarations (Proxy @FolderNode)
        , getTypeScriptDeclarations (Proxy @DailyNote)
        , getTypeScriptDeclarations (Proxy @AppView)
        ]

    header :: String
    header =
      Data.List.unlines
        [ "/**"
        , " * AUTO-GENERATED TypeScript types from Haskell."
        , " * DO NOT EDIT MANUALLY."
        , " *"
        , " * Regenerate with: cabal run generate-types > frontend/src/types.ts"
        , " * Or: just generate-types"
        , " *"
        , " * Source Haskell files:"
        , " *   - Task, TaskStatus: packages/ob/src/Ob/Task.hs"
        , " *   - Priority, TaskProperties: packages/ob/src/Ob/Task/Properties.hs"
        , " *   - FolderNode: packages/imako/src/Imako/Core/FolderTree.hs"
        , " *   - Filter: packages/imako/src/Imako/Core/Filter.hs"
        , " *   - DailyNote: packages/ob/src/Ob/DailyNotes.hs"
        , " *   - AppView: packages/imako/src/Imako/Core.hs"
        , " */"
        , ""
        ]
