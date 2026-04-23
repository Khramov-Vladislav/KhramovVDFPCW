{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Storage
  ( AppError (..),
    renderAppError,
    loadAppData,
    savePayrollCsv,
  )
where

import qualified Data.Aeson as A
import Data.Aeson (FromJSON (parseJSON), eitherDecodeFileStrict')
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( DefaultOrdered (headerOrder),
    FromField (parseField),
    FromNamedRecord (parseNamedRecord),
    ToNamedRecord (toNamedRecord),
    decodeByName,
    encodeDefaultOrderedByName,
    namedRecord,
    (.:),
    (.=),
  )
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Time (Day)
import Domain.Types
import Infrastructure.Paths
import System.Directory (doesFileExist)
import Utils.DateUtils (parseDayText)

data AppError
  = FileNotFound FilePath
  | ParseError String
  | ValidationError String
  deriving (Eq, Show)

renderAppError :: AppError -> String
renderAppError err =
  case err of
    FileNotFound fp -> "File not found: " <> fp
    ParseError msg -> "Parse error: " <> msg
    ValidationError msg -> "Validation error: " <> msg

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

instance FromNamedRecord TaskType where
  parseNamedRecord rec =
    TaskType
      <$> rec .: "task_id"
      <*> rec .: "complexity_coeff"

instance FromJSON BonusTier where
  parseJSON = A.withObject "BonusTier" $ \o ->
    BonusTier <$> o A..: "threshold" <*> o A..: "bonusPercent"

instance FromJSON Config where
  parseJSON = A.withObject "Config" $ \o ->
    Config
      <$> o A..: "baseKpi"
      <*> o A..: "bonusScale"
      <*> o A..: "taxRate"
      <*> o A..: "socialRate"

instance ToNamedRecord PayrollRow where
  toNamedRecord row =
    let emp = pyEmployee row
     in namedRecord
          [ "employee_id" .= employeeId emp,
            "full_name" .= employeeName emp,
            "position" .= employeePosition emp,
            "kpi_coeff" .= pyKpiCoeff row,
            "worked_hours" .= pyWorkedHours row,
            "gross_base" .= pyGrossBase row,
            "bonus_amount" .= pyBonusAmount row,
            "gross_total" .= pyGrossTotal row,
            "deductions" .= pyDeductions row,
            "net_salary" .= pyNetSalary row
          ]

instance DefaultOrdered PayrollRow where
  headerOrder _ =
    V.fromList $
      map
        B8.pack
        ["employee_id", "full_name", "position", "kpi_coeff", "worked_hours", "gross_base", "bonus_amount", "gross_total", "deductions", "net_salary"]

loadAppData :: IO (Either AppError AppData)
loadAppData = do
  eEmployees <- loadCsv "employees" employeesPath
  eTasks <- loadCsv "tasks" tasksPath
  eTaskTypes <- loadCsv "task_types" taskTypesPath
  eCalendar <- loadCalendar calendarPath
  eConfig <- loadConfig configPath
  pure $
    AppData
      <$> eEmployees
      <*> eTasks
      <*> (M.fromList . map (\t -> (taskTypeId t, t)) <$> eTaskTypes)
      <*> eCalendar
      <*> eConfig

savePayrollCsv :: FilePath -> [PayrollRow] -> IO ()
savePayrollCsv fp rows = BL.writeFile fp (encodeDefaultOrderedByName rows)

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

loadConfig :: FilePath -> IO (Either AppError Config)
loadConfig fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left (FileNotFound fp)
    else do
      decoded <- eitherDecodeFileStrict' fp
      pure $ either (Left . ParseError) Right decoded

loadCsv :: FromNamedRecord a => String -> FilePath -> IO (Either AppError [a])
loadCsv label fp = do
  exists <- doesFileExist fp
  if not exists
    then pure $ Left (FileNotFound fp)
    else do
      content <- BL.readFile fp
      pure $
        case decodeByName content of
          Left err -> Left (ParseError ("Could not parse " <> label <> ": " <> err))
          Right (_, rows) -> Right (V.toList rows)
