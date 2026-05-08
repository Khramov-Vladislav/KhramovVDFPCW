module Utils.DateUtils
  ( parseDayText,
    monthPrefix,
    parseYearMonth,
  )
where

import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)
import Text.Read (readMaybe)

parseDayText :: String -> Maybe Day
parseDayText = parseTimeM True defaultTimeLocale "%Y-%m-%d"

monthPrefix :: Day -> String
monthPrefix = formatTime defaultTimeLocale "%Y-%m"

parseYearMonth :: String -> Maybe (Integer, Int)
parseYearMonth value =
  if length value == 7 && value !! 4 == '-'
    then do
      y <- readMaybe (take 4 value)
      m <- readMaybe (drop 5 value)
      if m >= 1 && m <= 12 then pure (y, m) else Nothing
    else Nothing
