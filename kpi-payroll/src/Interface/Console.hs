module Interface.Console
  ( runProgram,
  )
where

import Application.UseCases
import Domain.Types
import Infrastructure.Logger (logError, logInfo)
import Infrastructure.Paths (outputDir)
import Infrastructure.Storage (renderAppError, savePayrollCsv)
import System.Directory (createDirectoryIfMissing)
import Utils.Validation (validateMonth)

runProgram :: IO ()
runProgram = do
  initialized <- initialize
  case initialized of
    Left err -> do
      let msg = renderAppError err
      putStrLn ("Initialization failed: " <> msg)
      logError ("Initialization failed: " <> msg)
    Right appData -> do
      logInfo "Application started"
      mainMenu appData

mainMenu :: AppData -> IO ()
mainMenu appData = do
  putStrLn ""
  putStrLn "=== KPI Payroll ==="
  putStrLn "1. Calculate payroll for month (YYYY-MM)"
  putStrLn "2. Show top productivity for month (YYYY-MM)"
  putStrLn "3. Export payroll CSV for month (YYYY-MM)"
  putStrLn "0. Exit"
  putStr "Choose action: "
  cmd <- getLine
  case cmd of
    "1" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          printPayroll (calculateMonthPayroll appData m)
          logInfo ("Calculated payroll for " <> m)
          mainMenu appData
    "2" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          printTop (calculateMonthPayroll appData m)
          logInfo ("Displayed top productivity for " <> m)
          mainMenu appData
    "3" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          exportMonth m (calculateMonthPayroll appData m)
          mainMenu appData
    "0" -> do
      logInfo "Application exited"
      putStrLn "Bye!"
    _ -> do
      putStrLn "Unknown command"
      logError ("Unknown command: " <> cmd)
      mainMenu appData

askMonth :: IO (Maybe String)
askMonth = do
  putStr "Enter month (YYYY-MM): "
  month <- getLine
  if validateMonth month
    then pure (Just month)
    else do
      putStrLn "Invalid format. Expected YYYY-MM."
      logError ("Invalid month input: " <> month)
      pure Nothing

printPayroll :: MonthResult -> IO ()
printPayroll res = do
  putStrLn "Employee | KPI | Hours | Net salary"
  mapM_
    ( \row ->
        putStrLn
          ( employeeName (pyEmployee row)
              <> " | "
              <> show (pyKpiCoeff row)
              <> " | "
              <> show (pyWorkedHours row)
              <> " | "
              <> show (pyNetSalary row)
          )
    )
    (mrPayrollRows res)

printTop :: MonthResult -> IO ()
printTop res = do
  putStrLn "Top productivity employees:"
  mapM_
    ( \row ->
        putStrLn
          ( employeeName (prEmployee row)
              <> ": score="
              <> show (prScore row)
              <> ", kpi="
              <> show (prKpiCoeff row)
          )
    )
    (topProductivity 10 (mrProductivityRows res))

exportMonth :: String -> MonthResult -> IO ()
exportMonth monthText res = do
  createDirectoryIfMissing True outputDir
  let fp = outputDir <> "/payroll-" <> monthText <> ".csv"
  savePayrollCsv fp (mrPayrollRows res)
  logInfo ("Exported payroll to " <> fp)
  putStrLn ("Saved: " <> fp)
