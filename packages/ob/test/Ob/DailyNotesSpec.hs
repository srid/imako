module Ob.DailyNotesSpec where

import Data.Time (fromGregorian)
import Ob.DailyNotes
import Test.Hspec

spec :: Spec
spec = do
  describe "momentToHaskellFormat" $ do
    it "converts standard YYYY-MM-DD format" $ do
      momentToHaskellFormat "YYYY-MM-DD" `shouldBe` "%Y-%m-%d"

    it "converts YYYY/MM/DD with slashes" $ do
      momentToHaskellFormat "YYYY/MM/DD" `shouldBe` "%Y/%m/%d"

    it "converts DD-MM-YYYY format" $ do
      momentToHaskellFormat "DD-MM-YYYY" `shouldBe` "%d-%m-%Y"

    it "handles single-digit day (D)" $ do
      momentToHaskellFormat "YYYY-MM-D" `shouldBe` "%Y-%m-%-d"

    it "converts short year YY" $ do
      momentToHaskellFormat "YY-MM-DD" `shouldBe` "%y-%m-%d"

    it "converts month names MMM and MMMM" $ do
      momentToHaskellFormat "YYYY-MMM-DD" `shouldBe` "%Y-%b-%d"
      momentToHaskellFormat "YYYY-MMMM-DD" `shouldBe` "%Y-%B-%d"

  describe "isDailyNote" $ do
    let configNoFolder = DailyNotesConfig Nothing (Just "YYYY-MM-DD")
    let configWithFolder = DailyNotesConfig (Just "Journal") (Just "YYYY-MM-DD")

    it "matches files in root when no folder configured" $ do
      isDailyNote configNoFolder "2025-11-29.md" `shouldBe` True
      isDailyNote configNoFolder "2024-01-15.md" `shouldBe` True

    it "rejects files with wrong date format" $ do
      isDailyNote configNoFolder "random-note.md" `shouldBe` False
      isDailyNote configNoFolder "2025-1-29.md" `shouldBe` False -- single digit month
      isDailyNote configNoFolder "29-11-2025.md" `shouldBe` False -- wrong order
    it "matches files in configured folder" $ do
      isDailyNote configWithFolder "Journal/2025-11-29.md" `shouldBe` True
      isDailyNote configWithFolder "Journal/2024-01-15.md" `shouldBe` True

    it "rejects files in wrong folder" $ do
      isDailyNote configWithFolder "2025-11-29.md" `shouldBe` False
      isDailyNote configWithFolder "Notes/2025-11-29.md" `shouldBe` False

    it "handles current directory notation" $ do
      -- ./path gets normalized to . by takeDirectory, which matches empty folder
      isDailyNote configNoFolder "./2025-11-29.md" `shouldBe` True

    it "uses default format when None" $ do
      let configDefault = DailyNotesConfig Nothing Nothing
      isDailyNote configDefault "2025-11-29.md" `shouldBe` True

  describe "parseDailyNoteDate" $ do
    let config = DailyNotesConfig Nothing (Just "YYYY-MM-DD")

    it "parses valid date from filename" $ do
      parseDailyNoteDate config "2025-11-29.md" `shouldBe` Just (fromGregorian 2025 11 29)
      parseDailyNoteDate config "2024-01-01.md" `shouldBe` Just (fromGregorian 2024 1 1)

    it "parses date from path with folder" $ do
      parseDailyNoteDate config "Journal/2025-11-29.md" `shouldBe` Just (fromGregorian 2025 11 29)

    it "returns Nothing for invalid dates" $ do
      parseDailyNoteDate config "random-note.md" `shouldBe` Nothing
      parseDailyNoteDate config "not-a-date.md" `shouldBe` Nothing

    it "handles different date formats" $ do
      let euroConfig = DailyNotesConfig Nothing (Just "DD-MM-YYYY")
      parseDailyNoteDate euroConfig "29-11-2025.md" `shouldBe` Just (fromGregorian 2025 11 29)

  describe "getTodayNotePath" $ do
    let day = fromGregorian 2025 11 29

    it "generates path without folder" $ do
      let config = DailyNotesConfig Nothing (Just "YYYY-MM-DD")
      getTodayNotePath config day `shouldBe` "2025-11-29.md"

    it "generates path with folder" $ do
      let config = DailyNotesConfig (Just "Journal") (Just "YYYY-MM-DD")
      getTodayNotePath config day `shouldBe` "Journal/2025-11-29.md"

    it "handles different date formats" $ do
      let config = DailyNotesConfig Nothing (Just "DD-MM-YYYY")
      getTodayNotePath config day `shouldBe` "29-11-2025.md"

    it "handles format with slashes" $ do
      let config = DailyNotesConfig Nothing (Just "YYYY/MM/DD")
      getTodayNotePath config day `shouldBe` "2025/11/29.md"

    it "uses default format when None" $ do
      let config = DailyNotesConfig Nothing Nothing
      getTodayNotePath config day `shouldBe` "2025-11-29.md"
