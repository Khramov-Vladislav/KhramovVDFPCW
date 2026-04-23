module Lib
  ( someFunc,
  )
where

import Interface.Console (runProgram)

someFunc :: IO ()
someFunc = runProgram
