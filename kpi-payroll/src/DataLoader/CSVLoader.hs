{-# LANGUAGE OverloadedStrings #-}

module DataLoader.CSVLoader
  ( loadEmployees,
    loadTasks,
    loadAbsences,
    loadCalendar,
    readCsvByName
  )
where

import Core.Common (AppError (..))
import Core.Types
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( FromField (parseField),
    FromNamedRecord (parseNamedRecord),
    decodeByName,
    (.:)
  )
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Time (Day)
import System.Directory (doesFileExist)
import Utils.DateUtils (parseDayText)

instance FromField TaskResult where
  parseField value =
    case value of
      "success" -> pure Success
      "partial" -> pure Partial
      "failed" -> pure Failed
      _ -> fail "Task result must be success|partial|failed"

instance FromNamedRecord Employee where
  parseNamedRecord rec =
    Employee
      <$> rec .: "employee_id"
      <*> rec .: "full_name"
      <*> rec .: "position"
      <*> rec .: "base_rate"
      <*> rec .: "norm"

instance FromNamedRecord TaskRecord where
  parseNamedRecord rec = do
    rawDay <- rec .: "date"
    day <- maybe (fail "Invalid date format, expected YYYY-MM-DD") pure (parseDayText rawDay)
    TaskRecord
      <$> rec .: "employee_id"
      <*> pure day
      <*> rec .: "task_id"
      <*> rec .: "complexity"
      <*> rec .: "planned_hours"
      <*> rec .: "actual_hours"
      <*> rec .: "result"

instance FromNamedRecord AbsenceRecord where
  parseNamedRecord rec =
    AbsenceRecord
      <$> rec .: "employee_id"
      <*> rec .: "month"
      <*> rec .: "sick_days"
      <*> rec .: "unpaid_days"

readCsvByName :: FromNamedRecord a => String -> FilePath -> IO (Either AppError [a])
readCsvByName label fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left (FileNotFound fp)
    else do
      content <- BL.readFile fp
      pure $
        case decodeByName content of
          Left err -> Left (ParseError ("Could not parse " <> label <> ": " <> err))
          Right (_, rows) -> Right (V.toList rows)

loadEmployees :: FilePath -> IO (Either AppError [Employee])
loadEmployees = readCsvByName "employees"

loadTasks :: FilePath -> IO (Either AppError [TaskRecord])
loadTasks = readCsvByName "tasks"

loadAbsences :: FilePath -> IO (Either AppError [AbsenceRecord])
loadAbsences = readCsvByName "absences"

loadCalendar :: FilePath -> IO (Either AppError (S.Set Day))
loadCalendar fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left (FileNotFound fp)
    else do
      raw <- readFile fp
      let nonEmpty = filter (not . null) (lines raw)
          parsed = map (\txt -> maybe (Left txt) Right (parseDayText txt)) nonEmpty
      pure $
        case [x | Left x <- parsed] of
          [] -> Right (S.fromList [x | Right x <- parsed])
          bad -> Left (ParseError ("Invalid calendar dates: " <> show bad))
