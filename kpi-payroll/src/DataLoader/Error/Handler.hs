{-# LANGUAGE OverloadedStrings #-}

module DataLoader.Error.Handler
  ( loadAppData,
    savePayrollCsv
  )
where

import Core.Common (AppError)
import Core.Types
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.Csv
  ( DefaultOrdered (headerOrder),
    ToNamedRecord (toNamedRecord),
    encodeDefaultOrderedByName,
    namedRecord,
    (.=)
  )
import DataLoader.CSVLoader (loadCalendar, loadEmployees, loadTasks)
import DataLoader.ConfigLoader (loadConfig)
import qualified Data.Vector as V

employeesPath, tasksPath, calendarPath, configPath :: FilePath
employeesPath = "data/employees.csv"
tasksPath = "data/tasks.csv"
calendarPath = "data/calendar.txt"
configPath = "data/config.json"

instance ToNamedRecord PayrollRow where
  toNamedRecord row =
    let emp = pyEmployee row
     in namedRecord
          [ "employee_id" .= employeeId emp,
            "full_name" .= employeeName emp,
            "position" .= employeePosition emp,
            "kpi_coeff" .= pyKpiCoeff row,
            "worked_hours" .= pyWorkedHours row,
            "month_status" .= (if pyIsPartialMonth row then ("неполный месяц" :: String) else ("полный месяц" :: String)),
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
        [ "employee_id",
          "full_name",
          "position",
          "kpi_coeff",
          "worked_hours",
          "month_status",
          "gross_base",
          "bonus_amount",
          "gross_total",
          "deductions",
          "net_salary"
        ]

loadAppData :: IO (Either AppError AppData)
loadAppData = do
  eEmployees <- loadEmployees employeesPath
  eTasks <- loadTasks tasksPath
  eCalendar <- loadCalendar calendarPath
  eConfig <- loadConfig configPath
  pure $
    AppData
      <$> eEmployees
      <*> eTasks
      <*> eCalendar
      <*> eConfig

savePayrollCsv :: FilePath -> [PayrollRow] -> IO ()
savePayrollCsv fp rows = BL.writeFile fp (encodeDefaultOrderedByName rows)
