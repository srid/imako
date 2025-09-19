{-# LANGUAGE OverloadedRecordDot #-}

module Ob.TaskSpec where

import Ob.Markdown (parseMarkdown)
import Ob.Task
import Test.Hspec

spec :: Spec
spec = do
  describe "extractTasks" $ do
    it "parses task lists from markdown" $ do
      let markdownContent =
            unlines
              [ "# Sample Tasks"
              , ""
              , "- [ ] Incomplete task"
              , "- [X] Completed task"
              , "- [ ] Another incomplete task"
              ]

      case parseMarkdown "test.md" markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks "test.md" pandoc
          map (\t -> (extractText t.description, t.isCompleted)) tasks
            `shouldBe` [ ("Incomplete task", False)
                       , ("Completed task", True)
                       , ("Another incomplete task", False)
                       ]
