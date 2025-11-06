{- | Recurrence rules for obsidian-tasks plugin.

TODO: Extract this as a separate library: Haskell version of https://github.com/jkbrzt/rrule
-}
module Ob.Task.Recurrence (
  Recurrence (..),
  RecurrenceRule (..),
  MonthConstraint (..),
  DayOfWeekConstraint (..),
  DayOfWeek (..),
  Month (..),
  parseRecurrence,
  formatRecurrence,
) where

import Data.Text qualified as T

-- | Represents a task recurrence rule
data Recurrence = Recurrence
  { rule :: RecurrenceRule
  , whenDone :: Bool
  -- ^ If True, next occurrence is calculated from completion date rather than original date
  }
  deriving (Show, Eq)

-- | The core recurrence pattern
data RecurrenceRule
  = EveryNDays Int
  | EveryWeekday
  | EveryWeek (Maybe [DayOfWeek])
  | EveryNWeeks Int (Maybe [DayOfWeek])
  | EveryNMonths Int (Maybe MonthConstraint)
  | EveryMonth (Maybe MonthConstraint)
  | EveryMonthsInYear [Month] (Maybe MonthConstraint)
  | EveryYear
  deriving (Show, Eq)

-- | Constraints for month-based recurrence
data MonthConstraint
  = -- | On the Nth day of month (1st, 2nd, etc)
    OnThe Int
  | -- | On the last day of the month
    OnTheLast
  | -- | On the Nth-to-last day (2nd last, 3rd last, etc)
    OnTheNthLast Int
  | -- | On a specific day of week occurrence
    OnTheDayOfWeek DayOfWeekConstraint
  | -- | On specific days (e.g., 1st and 24th)
    OnTheDays [Int]
  deriving (Show, Eq)

-- | Day of week constraints for monthly patterns
data DayOfWeekConstraint
  = LastDayOfWeek DayOfWeek
  | NthDayOfWeek Int DayOfWeek
  | NthLastDayOfWeek Int DayOfWeek
  deriving (Show, Eq)

-- | Days of the week
data DayOfWeek = Mon | Tue | Wed | Thu | Fri | Sat | Sun
  deriving (Show, Eq)

-- | Months of the year
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show, Eq)

-- | Parse recurrence rule from text after 🔁 emoji
parseRecurrence :: Text -> Maybe Recurrence
parseRecurrence input =
  let (ruleText, whenDoneFlag) = extractWhenDone input
      normalizedRule = T.strip ruleText
   in case parseRule normalizedRule of
        Just r -> Just $ Recurrence r whenDoneFlag
        Nothing -> Nothing
  where
    extractWhenDone :: Text -> (Text, Bool)
    extractWhenDone txt =
      if "when done" `T.isSuffixOf` T.toLower txt
        then (T.strip $ T.dropEnd 9 txt, True)
        else (txt, False)

-- | Parse the core recurrence rule
parseRule :: Text -> Maybe RecurrenceRule
parseRule txt
  | not ("every" `T.isPrefixOf` T.toLower txt) = Nothing
  | otherwise = parseEvery (T.strip $ T.drop 5 txt)
  where
    parseEvery :: Text -> Maybe RecurrenceRule
    parseEvery rest = case words rest of
      -- every weekday
      ["weekday"] -> Just EveryWeekday
      -- every year
      ["year"] -> Just EveryYear
      -- every N days
      [n, "days"] -> EveryNDays <$> readMaybeInt n
      [n, "day"] -> EveryNDays <$> readMaybeInt n
      -- every week [on DayOfWeek[, DayOfWeek]*]
      ("week" : daysRest) -> Just $ EveryWeek (parseDaysOfWeek daysRest)
      -- every N weeks [on DayOfWeek[, DayOfWeek]*]
      (n : "weeks" : daysRest) -> do
        num <- readMaybeInt n
        pure $ EveryNWeeks num (parseDaysOfWeek daysRest)
      -- every month [on constraint]
      ("month" : constraintRest) -> Just $ EveryMonth (parseMonthConstraint constraintRest)
      -- every N months [on constraint]
      (n : "months" : constraintRest) -> do
        num <- readMaybeInt n
        pure $ EveryNMonths num (parseMonthConstraint constraintRest)
      -- every DayOfWeek (shorthand for every week on DayOfWeek)
      [day] -> case parseDayOfWeek day of
        Just dow -> Just $ EveryWeek (Just [dow])
        Nothing -> parseMonthsInYear [day]
      -- every Month [and Month]* [on constraint]
      ws -> parseMonthsInYear ws

    parseDaysOfWeek :: [Text] -> Maybe [DayOfWeek]
    parseDaysOfWeek [] = Nothing
    parseDaysOfWeek ("on" : rest) = parseDaysOfWeek rest
    parseDaysOfWeek ws =
      let days = mapMaybe parseDayOfWeek (splitOnComma ws)
       in if null days then Nothing else Just days

    parseMonthConstraint :: [Text] -> Maybe MonthConstraint
    parseMonthConstraint [] = Nothing
    parseMonthConstraint ("on" : rest) = parseMonthConstraint rest
    parseMonthConstraint ["the", "last"] = Just OnTheLast
    parseMonthConstraint ["the", "last", day] = OnTheDayOfWeek . LastDayOfWeek <$> parseDayOfWeek day
    parseMonthConstraint ["the", nth, "last", day] = do
      n <- parseOrdinal nth
      dow <- parseDayOfWeek day
      pure $ OnTheDayOfWeek (NthLastDayOfWeek n dow)
    parseMonthConstraint ["the", nth] = OnThe <$> parseOrdinal nth
    parseMonthConstraint ["the", nth, day] = do
      n <- parseOrdinal nth
      dow <- parseDayOfWeek day
      pure $ OnTheDayOfWeek (NthDayOfWeek n dow)
    parseMonthConstraint ws =
      let days = mapMaybe parseOrdinal (filter (/= "and") $ splitOnComma ws)
       in if null days then Nothing else Just (OnTheDays days)

    parseMonthsInYear :: [Text] -> Maybe RecurrenceRule
    parseMonthsInYear ws =
      let (monthWords, constraintWords) = span (\w -> T.toLower w /= "on") ws
          months = mapMaybe parseMonth (splitOnComma monthWords)
          constraint = if null constraintWords then Nothing else parseMonthConstraint constraintWords
       in if null months
            then Nothing
            else Just $ EveryMonthsInYear months constraint

    parseDayOfWeek :: Text -> Maybe DayOfWeek
    parseDayOfWeek w = case T.toLower w of
      "monday" -> Just Mon
      "tuesday" -> Just Tue
      "wednesday" -> Just Wed
      "thursday" -> Just Thu
      "friday" -> Just Fri
      "saturday" -> Just Sat
      "sunday" -> Just Sun
      _ -> Nothing

    parseMonth :: Text -> Maybe Month
    parseMonth w = case T.toLower w of
      "january" -> Just Jan
      "february" -> Just Feb
      "march" -> Just Mar
      "april" -> Just Apr
      "may" -> Just May
      "june" -> Just Jun
      "july" -> Just Jul
      "august" -> Just Aug
      "september" -> Just Sep
      "october" -> Just Oct
      "november" -> Just Nov
      "december" -> Just Dec
      _ -> Nothing

    parseOrdinal :: Text -> Maybe Int
    parseOrdinal w = case T.toLower w of
      "1st" -> Just 1
      "2nd" -> Just 2
      "3rd" -> Just 3
      "last" -> Just 31 -- Special marker for last day
      _ | "th" `T.isSuffixOf` T.toLower w -> readMaybeInt (T.dropEnd 2 w)
      _ -> readMaybeInt w

    readMaybeInt :: Text -> Maybe Int
    readMaybeInt t = case reads (toString t) of
      [(n, "")] | n > 0 -> Just n
      _ -> Nothing

    splitOnComma :: [Text] -> [Text]
    splitOnComma = concatMap (T.splitOn ",") >>> map T.strip >>> filter (not . T.null)

-- | Format recurrence rule back to human-readable text
formatRecurrence :: Recurrence -> Text
formatRecurrence (Recurrence r wd) =
  let ruleText = formatRule r
      whenDoneText = if wd then " when done" else ""
   in ruleText <> whenDoneText
  where
    formatRule :: RecurrenceRule -> Text
    formatRule = \case
      EveryNDays 1 -> "every day"
      EveryNDays n -> "every " <> show n <> " days"
      EveryWeekday -> "every weekday"
      EveryWeek Nothing -> "every week"
      EveryWeek (Just days) -> "every week on " <> formatDays days
      EveryNWeeks n Nothing -> "every " <> show n <> " weeks"
      EveryNWeeks n (Just days) -> "every " <> show n <> " weeks on " <> formatDays days
      EveryMonth Nothing -> "every month"
      EveryMonth (Just c) -> "every month on " <> formatMonthConstraint c
      EveryNMonths n Nothing -> "every " <> show n <> " months"
      EveryNMonths n (Just c) -> "every " <> show n <> " months on " <> formatMonthConstraint c
      EveryMonthsInYear months Nothing -> "every " <> formatMonths months
      EveryMonthsInYear months (Just c) -> "every " <> formatMonths months <> " on " <> formatMonthConstraint c
      EveryYear -> "every year"

    formatDays :: [DayOfWeek] -> Text
    formatDays = map formatDay >>> T.intercalate ", "

    formatDay :: DayOfWeek -> Text
    formatDay = \case
      Mon -> "Monday"
      Tue -> "Tuesday"
      Wed -> "Wednesday"
      Thu -> "Thursday"
      Fri -> "Friday"
      Sat -> "Saturday"
      Sun -> "Sunday"

    formatMonths :: [Month] -> Text
    formatMonths = map formatMonth >>> T.intercalate " and "

    formatMonth :: Month -> Text
    formatMonth = \case
      Jan -> "January"
      Feb -> "February"
      Mar -> "March"
      Apr -> "April"
      May -> "May"
      Jun -> "June"
      Jul -> "July"
      Aug -> "August"
      Sep -> "September"
      Oct -> "October"
      Nov -> "November"
      Dec -> "December"

    formatMonthConstraint :: MonthConstraint -> Text
    formatMonthConstraint = \case
      OnThe n -> "the " <> formatOrdinal n
      OnTheLast -> "the last"
      OnTheNthLast n -> "the " <> formatOrdinal n <> " last"
      OnTheDayOfWeek (LastDayOfWeek d) -> "the last " <> formatDay d
      OnTheDayOfWeek (NthDayOfWeek n d) -> "the " <> formatOrdinal n <> " " <> formatDay d
      OnTheDayOfWeek (NthLastDayOfWeek n d) -> "the " <> formatOrdinal n <> " last " <> formatDay d
      OnTheDays days -> T.intercalate " and " (map formatOrdinal days)

    formatOrdinal :: Int -> Text
    formatOrdinal n
      | n == 1 = "1st"
      | n == 2 = "2nd"
      | n == 3 = "3rd"
      | n >= 4 && n <= 20 = show n <> "th"
      | n `mod` 10 == 1 = show n <> "st"
      | n `mod` 10 == 2 = show n <> "nd"
      | n `mod` 10 == 3 = show n <> "rd"
      | otherwise = show n <> "th"
