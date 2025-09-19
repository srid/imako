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
          length tasks `shouldBe` 3

          let taskTexts = map (extractText . description) tasks
              completions = map isCompleted tasks

          taskTexts `shouldBe` ["Incomplete task", "Completed task", "Another incomplete task"]
          completions `shouldBe` [False, True, False]
