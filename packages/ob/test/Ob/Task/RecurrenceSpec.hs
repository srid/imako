module Ob.Task.RecurrenceSpec where

import Ob.Task.Recurrence
import Test.Hspec

spec :: Spec
spec = do
  describe "parseRecurrence" $ do
    describe "basic patterns" $ do
      it "parses 'every 1 day'" $ do
        parseRecurrence "every 1 day"
          `shouldBe` Just (Recurrence (EveryNDays 1) False)

      it "parses 'every weekday'" $ do
        parseRecurrence "every weekday"
          `shouldBe` Just (Recurrence EveryWeekday False)

      it "parses 'every week'" $ do
        parseRecurrence "every week"
          `shouldBe` Just (Recurrence (EveryWeek Nothing) False)

      it "parses 'every month'" $ do
        parseRecurrence "every month"
          `shouldBe` Just (Recurrence (EveryMonth Nothing) False)

      it "parses 'every year'" $ do
        parseRecurrence "every year"
          `shouldBe` Just (Recurrence EveryYear False)

    describe "N days/weeks/months patterns" $ do
      it "parses 'every 2 days'" $ do
        parseRecurrence "every 2 days"
          `shouldBe` Just (Recurrence (EveryNDays 2) False)

      it "parses 'every 3 weeks'" $ do
        parseRecurrence "every 3 weeks"
          `shouldBe` Just (Recurrence (EveryNWeeks 3 Nothing) False)

      it "parses 'every 6 months'" $ do
        parseRecurrence "every 6 months"
          `shouldBe` Just (Recurrence (EveryNMonths 6 Nothing) False)

    describe "day of week patterns" $ do
      it "parses 'every Sunday' as shorthand" $ do
        parseRecurrence "every Sunday"
          `shouldBe` Just (Recurrence (EveryWeek (Just [Sun])) False)

      it "parses 'every week on Monday'" $ do
        parseRecurrence "every week on Monday"
          `shouldBe` Just (Recurrence (EveryWeek (Just [Mon])) False)

      it "parses 'every week on Monday, Wednesday, Friday'" $ do
        parseRecurrence "every week on Monday, Wednesday, Friday"
          `shouldBe` Just (Recurrence (EveryWeek (Just [Mon, Wed, Fri])) False)

      it "parses 'every week on Saturday and Sunday'" $ do
        parseRecurrence "every week on Saturday and Sunday"
          `shouldBe` Just (Recurrence (EveryWeek (Just [Sat, Sun])) False)

      it "parses 'every 2 weeks on Tuesday'" $ do
        parseRecurrence "every 2 weeks on Tuesday"
          `shouldBe` Just (Recurrence (EveryNWeeks 2 (Just [Tue])) False)

    describe "month constraint patterns" $ do
      it "parses 'every month on the 1st'" $ do
        parseRecurrence "every month on the 1st"
          `shouldBe` Just (Recurrence (EveryMonth (Just (OnThe 1))) False)

      it "parses 'every month on the 15th'" $ do
        parseRecurrence "every month on the 15th"
          `shouldBe` Just (Recurrence (EveryMonth (Just (OnThe 15))) False)

      it "parses 'every month on the last'" $ do
        parseRecurrence "every month on the last"
          `shouldBe` Just (Recurrence (EveryMonth (Just OnTheLast)) False)

      it "parses 'every month on the last Monday'" $ do
        parseRecurrence "every month on the last Monday"
          `shouldBe` Just (Recurrence (EveryMonth (Just (OnTheDayOfWeek (LastDayOfWeek Mon)))) False)

      it "parses 'every month on the 2nd Tuesday'" $ do
        parseRecurrence "every month on the 2nd Tuesday"
          `shouldBe` Just (Recurrence (EveryMonth (Just (OnTheDayOfWeek (NthDayOfWeek 2 Tue)))) False)

      it "parses 'every 3 months on the 10th'" $ do
        parseRecurrence "every 3 months on the 10th"
          `shouldBe` Just (Recurrence (EveryNMonths 3 (Just (OnThe 10))) False)

    describe "month-specific patterns" $ do
      it "parses 'every January'" $ do
        parseRecurrence "every January"
          `shouldBe` Just (Recurrence (EveryMonthsInYear [Jan] Nothing) False)

      it "parses 'every January and July'" $ do
        parseRecurrence "every January and July"
          `shouldBe` Just (Recurrence (EveryMonthsInYear [Jan, Jul] Nothing) False)

    describe "when done modifier" $ do
      it "parses 'every week when done'" $ do
        parseRecurrence "every week when done"
          `shouldBe` Just (Recurrence (EveryWeek Nothing) True)

      it "parses 'every month on the 1st when done'" $ do
        parseRecurrence "every month on the 1st when done"
          `shouldBe` Just (Recurrence (EveryMonth (Just (OnThe 1))) True)

      it "parses 'every 2 days when done'" $ do
        parseRecurrence "every 2 days when done"
          `shouldBe` Just (Recurrence (EveryNDays 2) True)

    describe "case insensitivity" $ do
      it "parses 'EVERY WEEK'" $ do
        parseRecurrence "EVERY WEEK"
          `shouldBe` Just (Recurrence (EveryWeek Nothing) False)

      it "parses 'Every Monday'" $ do
        parseRecurrence "Every Monday"
          `shouldBe` Just (Recurrence (EveryWeek (Just [Mon])) False)

      it "parses 'every week WHEN DONE'" $ do
        parseRecurrence "every week WHEN DONE"
          `shouldBe` Just (Recurrence (EveryWeek Nothing) True)

    describe "invalid input" $ do
      it "returns Nothing for empty string" $ do
        parseRecurrence "" `shouldBe` Nothing

      it "returns Nothing for 'every'" $ do
        parseRecurrence "every" `shouldBe` Nothing

      it "returns Nothing for 'weekly'" $ do
        parseRecurrence "weekly" `shouldBe` Nothing

      it "returns Nothing for malformed input" $ do
        parseRecurrence "every fortnight" `shouldBe` Nothing

  describe "formatRecurrence" $ do
    it "formats 'every day'" $ do
      formatRecurrence (Recurrence (EveryNDays 1) False)
        `shouldBe` "every day"

    it "formats 'every 2 days'" $ do
      formatRecurrence (Recurrence (EveryNDays 2) False)
        `shouldBe` "every 2 days"

    it "formats 'every weekday'" $ do
      formatRecurrence (Recurrence EveryWeekday False)
        `shouldBe` "every weekday"

    it "formats 'every week'" $ do
      formatRecurrence (Recurrence (EveryWeek Nothing) False)
        `shouldBe` "every week"

    it "formats 'every week on Sunday'" $ do
      formatRecurrence (Recurrence (EveryWeek (Just [Sun])) False)
        `shouldBe` "every week on Sunday"

    it "formats 'every month'" $ do
      formatRecurrence (Recurrence (EveryMonth Nothing) False)
        `shouldBe` "every month"

    it "formats 'every month on the 1st'" $ do
      formatRecurrence (Recurrence (EveryMonth (Just (OnThe 1))) False)
        `shouldBe` "every month on the 1st"

    it "formats 'every month on the last'" $ do
      formatRecurrence (Recurrence (EveryMonth (Just OnTheLast)) False)
        `shouldBe` "every month on the last"

    it "formats 'every year'" $ do
      formatRecurrence (Recurrence EveryYear False)
        `shouldBe` "every year"

    it "formats 'when done' modifier" $ do
      formatRecurrence (Recurrence (EveryWeek Nothing) True)
        `shouldBe` "every week when done"

  describe "round-trip properties" $ do
    it "round-trips 'every week'" $ do
      let input = "every week"
      case parseRecurrence input of
        Nothing -> expectationFailure "Failed to parse"
        Just recur -> parseRecurrence (formatRecurrence recur) `shouldBe` Just recur

    it "round-trips 'every month on the 15th'" $ do
      let input = "every month on the 15th"
      case parseRecurrence input of
        Nothing -> expectationFailure "Failed to parse"
        Just recur -> parseRecurrence (formatRecurrence recur) `shouldBe` Just recur

    it "round-trips 'every week when done'" $ do
      let input = "every week when done"
      case parseRecurrence input of
        Nothing -> expectationFailure "Failed to parse"
        Just recur -> parseRecurrence (formatRecurrence recur) `shouldBe` Just recur

    it "round-trips 'every 3 months on the 10th'" $ do
      let input = "every 3 months on the 10th"
      case parseRecurrence input of
        Nothing -> expectationFailure "Failed to parse"
        Just recur -> parseRecurrence (formatRecurrence recur) `shouldBe` Just recur
