module Utils.Validation
  ( validateMonth,
  )
where

import Data.Char (isDigit)
import Text.Read (readMaybe)

validateMonth :: String -> Bool
validateMonth txt =
  case splitMonth txt of
    Just (year, month) -> year == 2025 && month >= 1 && month <= 12
    Nothing -> False
  where
    splitMonth :: String -> Maybe (Int, Int)
    splitMonth value =
      if length value == 7
           && all isDigit (take 4 value)
           && value !! 4 == '-'
           && all isDigit (drop 5 value)
        then do
          year <- readMaybe (take 4 value)
          month <- readMaybe (drop 5 value)
          pure (year, month :: Int)
        else Nothing
