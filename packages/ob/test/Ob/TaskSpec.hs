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
          testFilePath = "test.md"

      case parseMarkdown testFilePath markdownContent of
        Left err -> expectationFailure $ "Failed to parse markdown: " <> show err
        Right (_, pandoc) -> do
          let tasks = extractTasks testFilePath pandoc
          length tasks `shouldBe` 3

          let incompleteTasks = filter (not . isCompleted) tasks
          let completedTasks = filter isCompleted tasks

          length incompleteTasks `shouldBe` 2
          length completedTasks `shouldBe` 1

          case viaNonEmpty head incompleteTasks of
            Just task -> taskText task `shouldBe` "Incomplete task"
            Nothing -> expectationFailure "No incomplete tasks found"
          case viaNonEmpty head completedTasks of
            Just task -> taskText task `shouldBe` "Completed task"
            Nothing -> expectationFailure "No completed tasks found"
