module Utils.Validation
  ( validateMonth,
  )
where

import Data.Char (isDigit)

validateMonth :: String -> Bool
validateMonth value =
  length value == 7
    && all isDigit (take 4 value)
    && value !! 4 == '-'
    && all isDigit (drop 5 value)
    && let monthDigits = drop 5 value
           monthNum = read monthDigits :: Int
        in monthNum >= 1 && monthNum <= 12
