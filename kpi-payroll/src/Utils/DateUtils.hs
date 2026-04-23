module Utils.DateUtils
  ( parseDayText,
    monthPrefix,
  )
where

import Data.Time (Day, defaultTimeLocale, formatTime, parseTimeM)

parseDayText :: String -> Maybe Day
parseDayText = parseTimeM True defaultTimeLocale "%Y-%m-%d"

monthPrefix :: Day -> String
monthPrefix = formatTime defaultTimeLocale "%Y-%m"
