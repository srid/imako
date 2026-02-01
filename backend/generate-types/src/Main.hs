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
import Data.Bool qualified as B
import Data.List qualified
import Data.Maybe qualified as M
import Data.Time (Day)
import Imako.Core (AppView (..))
import Imako.Core.Filter (Filter (..))
import Imako.Core.FolderTree (FolderNode (..))
import Ob.DailyNotes (DailyNote (..))
import Ob.Task (Task (..), TaskStatus (..))
import Ob.Task.Properties (Priority (..))
import Text.Pandoc.Definition (Pandoc)

-- | Helper to create a required TSField (works around relude/base Bool conflict)
reqField :: String -> String -> TSField
reqField name typ = TSField B.False name typ M.Nothing

-- | Helper to create an optional TSField
optField :: String -> String -> TSField
optField name typ = TSField B.True name typ M.Nothing

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
            [ reqField "description" "string"
            , reqField "sourceNote" "string"
            , reqField "status" "TaskStatus"
            , optField "dueDate" "string"
            , optField "scheduledDate" "string"
            , optField "startDate" "string"
            , optField "completedDate" "string"
            , reqField "priority" "Priority"
            , reqField "tags" "string[]"
            , reqField "parentBreadcrumbs" "string[]"
            ]
        , interfaceDoc = M.Nothing
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
            [ reqField "day" "string" -- Day is serialized as ISO string
            , reqField "notePath" "string"
            ]
        , interfaceDoc = M.Nothing
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
