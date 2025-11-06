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
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

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

type Parser = Parsec Void Text

-- | Parse recurrence rule from text after 🔁 emoji
parseRecurrence :: Text -> Maybe Recurrence
parseRecurrence = parseMaybe recurrenceP

-- | Main recurrence parser
recurrenceP :: Parser Recurrence
recurrenceP = do
  r <- ruleP
  whenDoneFlag <- option False (space1 *> whenDoneP)
  space
  eof
  pure $ Recurrence r whenDoneFlag

-- | Parse "when done" modifier
whenDoneP :: Parser Bool
whenDoneP = string' "when" *> space1 *> string' "done" $> True

-- | Parse recurrence rule (starts with "every")
ruleP :: Parser RecurrenceRule
ruleP =
  string' "every"
    *> space1
    *> choice
      [ try weekdayP
      , try yearP
      , try nDaysP
      , try nWeeksP
      , try weekP
      , try nMonthsP
      , try monthP
      , try dayOfWeekShorthandP
      , monthsInYearP
      ]

-- | Parse "every weekday"
weekdayP :: Parser RecurrenceRule
weekdayP = string' "weekday" $> EveryWeekday

-- | Parse "every year"
yearP :: Parser RecurrenceRule
yearP = string' "year" $> EveryYear

-- | Parse "every N day(s)"
nDaysP :: Parser RecurrenceRule
nDaysP = do
  n <- L.decimal
  space1
  void $ string' "day" <* optional (char' 's')
  pure $ EveryNDays n

-- | Parse "every week [on DayOfWeek(s)]"
weekP :: Parser RecurrenceRule
weekP = do
  void $ string' "week"
  days <- optional (try $ space1 *> onP *> space1 *> dayListP)
  pure $ EveryWeek days

-- | Parse "every N weeks [on DayOfWeek(s)]"
nWeeksP :: Parser RecurrenceRule
nWeeksP = do
  n <- L.decimal
  space1
  void $ string' "week" <* char' 's'
  days <- optional (try $ space1 *> onP *> space1 *> dayListP)
  pure $ EveryNWeeks n days

-- | Parse "every month [on constraint]"
monthP :: Parser RecurrenceRule
monthP = do
  void $ string' "month"
  constraint <- optional (try $ space1 *> onP *> space1 *> monthConstraintP)
  pure $ EveryMonth constraint

-- | Parse "every N months [on constraint]"
nMonthsP :: Parser RecurrenceRule
nMonthsP = do
  n <- L.decimal
  space1
  void $ string' "month" <* char' 's'
  constraint <- optional (try $ space1 *> onP *> space1 *> monthConstraintP)
  pure $ EveryNMonths n constraint

-- | Parse "every DayOfWeek" as shorthand for "every week on DayOfWeek"
dayOfWeekShorthandP :: Parser RecurrenceRule
dayOfWeekShorthandP = do
  dow <- dayOfWeekP
  pure $ EveryWeek (Just [dow])

-- | Parse "every Month(s) [on constraint]"
monthsInYearP :: Parser RecurrenceRule
monthsInYearP = do
  months <- monthListP
  constraint <- optional (try $ space1 *> onP *> space1 *> monthConstraintP)
  pure $ EveryMonthsInYear months constraint

-- | Parse "on" keyword
onP :: Parser ()
onP = void $ string' "on"

-- | Parse comma/and-separated list of days of week
dayListP :: Parser [DayOfWeek]
dayListP = dayOfWeekP `sepBy1` separator
  where
    separator = try (space *> char' ',' *> space) <|> (space1 *> string' "and" *> space1)

-- | Parse comma/and-separated list of months
monthListP :: Parser [Month]
monthListP = monthNameP `sepBy1` separator
  where
    separator = try (space *> char' ',' *> space) <|> (space1 *> string' "and" *> space1)

-- | Parse a day of week
dayOfWeekP :: Parser DayOfWeek
dayOfWeekP =
  choice
    [ string' "monday" $> Mon
    , string' "tuesday" $> Tue
    , string' "wednesday" $> Wed
    , string' "thursday" $> Thu
    , string' "friday" $> Fri
    , string' "saturday" $> Sat
    , string' "sunday" $> Sun
    ]

-- | Parse a month name
monthNameP :: Parser Month
monthNameP =
  choice
    [ string' "january" $> Jan
    , string' "february" $> Feb
    , string' "march" $> Mar
    , string' "april" $> Apr
    , string' "may" $> May
    , string' "june" $> Jun
    , string' "july" $> Jul
    , string' "august" $> Aug
    , string' "september" $> Sep
    , string' "october" $> Oct
    , string' "november" $> Nov
    , string' "december" $> Dec
    ]

-- | Parse month constraints
monthConstraintP :: Parser MonthConstraint
monthConstraintP =
  choice
    [ try onTheLastDayOfWeekP
    , try onTheNthLastDayOfWeekP
    , try onTheNthDayOfWeekP
    , try onTheLastP
    , try onTheNthP
    , onTheDaysP
    ]

-- | Parse "the last"
onTheLastP :: Parser MonthConstraint
onTheLastP = string' "the" *> space1 *> string' "last" $> OnTheLast

-- | Parse "the last DayOfWeek"
onTheLastDayOfWeekP :: Parser MonthConstraint
onTheLastDayOfWeekP = do
  void $ string' "the" *> space1 *> string' "last" *> space1
  OnTheDayOfWeek . LastDayOfWeek <$> dayOfWeekP

-- | Parse "the Nth last DayOfWeek"
onTheNthLastDayOfWeekP :: Parser MonthConstraint
onTheNthLastDayOfWeekP = do
  void $ string' "the" *> space1
  n <- ordinalP
  void $ space1 *> string' "last" *> space1
  OnTheDayOfWeek . NthLastDayOfWeek n <$> dayOfWeekP

-- | Parse "the Nth DayOfWeek"
onTheNthDayOfWeekP :: Parser MonthConstraint
onTheNthDayOfWeekP = do
  void $ string' "the" *> space1
  n <- ordinalP
  void space1
  OnTheDayOfWeek . NthDayOfWeek n <$> dayOfWeekP

-- | Parse "the Nth"
onTheNthP :: Parser MonthConstraint
onTheNthP = do
  void $ string' "the" *> space1
  OnThe <$> ordinalP

-- | Parse "the Nth and Mth" (comma/and separated)
onTheDaysP :: Parser MonthConstraint
onTheDaysP = do
  void $ string' "the" *> space1
  days <- ordinalP `sepBy1` separator
  pure $ OnTheDays days
  where
    separator = try (space *> char' ',' *> space) <|> (space1 *> string' "and" *> space1)

-- | Parse ordinal number (1st, 2nd, 3rd, 4th, etc.)
ordinalP :: Parser Int
ordinalP =
  choice
    [ try $ string' "1st" $> 1
    , try $ string' "2nd" $> 2
    , try $ string' "3rd" $> 3
    , try $ string' "last" $> 31
    , try $ do
        n <- L.decimal
        void $ string' "th" <|> string' "st" <|> string' "nd" <|> string' "rd"
        pure n
    , L.decimal
    ]

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
