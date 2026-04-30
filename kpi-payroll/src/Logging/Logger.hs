module Logging.Logger
  ( logInfo,
    logError
  )
where

import Data.Time (defaultTimeLocale, formatTime, getCurrentTime)
import System.Directory (createDirectoryIfMissing)

logsDir :: FilePath
logsDir = "logs"

writeLog :: String -> String -> IO ()
writeLog level msg = do
  createDirectoryIfMissing True logsDir
  now <- getCurrentTime
  let ts = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" now
  appendFile (logsDir <> "/app.log") ("[" <> ts <> "] [" <> level <> "] " <> msg <> "\n")

logInfo :: String -> IO ()
logInfo = writeLog "INFO"

logError :: String -> IO ()
logError = writeLog "ERROR"
