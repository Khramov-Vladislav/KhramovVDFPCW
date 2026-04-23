module Application.UseCases
  ( MonthResult (..),
    initialize,
    calculateMonthPayroll,
    topProductivity,
  )
where

import Data.List (sortOn)
import Domain.Payroll (buildPayrollRows)
import Domain.Productivity (buildProductivityRows, filterTasksByMonth)
import Domain.Types
import Infrastructure.Storage (AppError, loadAppData)

data MonthResult = MonthResult
  { mrProductivityRows :: [ProductivityRow],
    mrPayrollRows :: [PayrollRow]
  }

initialize :: IO (Either AppError AppData)
initialize = loadAppData

calculateMonthPayroll :: AppData -> String -> MonthResult
calculateMonthPayroll appData monthText =
  let monthTasks = filterTasksByMonth monthText (adTasks appData)
      productivityRows = buildProductivityRows (adTaskTypes appData) (adEmployees appData) monthTasks
      payrollRows = buildPayrollRows (adConfig appData) productivityRows
   in MonthResult productivityRows payrollRows

topProductivity :: Int -> [ProductivityRow] -> [ProductivityRow]
topProductivity n rows = take n (reverse (sortOn prScore rows))
