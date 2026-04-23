module Utils.Validation
  ( validateMonth,
  )
where

import Data.Char (isDigit)

validateMonth :: String -> Bool
validateMonth txt =
  length txt == 7
    && all isDigit (take 4 txt)
    && txt !! 4 == '-'
    && all isDigit (drop 5 txt)
