module Main (main) where

import Core.Common (renderAppError)
import Core.Types
import DataLoader.Error.Handler (loadAppData, savePayrollCsv)
import Logging.Logger (logError, logInfo)
import Productivity.Calculator (buildProductivityRows, filterTasksByMonth, topProductivity)
import Salary.Calculator (buildPayrollRows)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Utils.Validation (validateMonth)

main :: IO ()
main = do
  initialized <- loadAppData
  case initialized of
    Left err -> do
      let msg = renderAppError err
      putStrLn ("Ошибка инициализации: " <> msg)
      logError ("Ошибка инициализации: " <> msg)
    Right appData -> do
      logInfo "Приложение запущено"
      mainMenu appData

mainMenu :: AppData -> IO ()
mainMenu appData = do
  putStrLn ""
  putStrLn "=== Учет KPI и расчет зарплаты ==="
  putStrLn "1. Рассчитать зарплату за месяц (ГГГГ-ММ)"
  putStrLn "2. Показать топ сотрудников по продуктивности (ГГГГ-ММ)"
  putStrLn "3. Выгрузить зарплату в CSV (ГГГГ-ММ)"
  putStrLn "0. Выход"
  putStrLn "Выберите действие:"
  putStr "> "
  cmd <- getLine
  case cmd of
    "1" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          printPayroll (monthResult appData m)
          logInfo ("Рассчитана зарплата за " <> m)
          mainMenu appData
    "2" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          printTop (monthResult appData m)
          logInfo ("Показан топ продуктивности за " <> m)
          mainMenu appData
    "3" -> do
      month <- askMonth
      case month of
        Nothing -> mainMenu appData
        Just m -> do
          exportMonth m (monthResult appData m)
          mainMenu appData
    "0" -> do
      logInfo "Приложение завершено"
      putStrLn "Работа завершена."
    _ -> do
      putStrLn "Неизвестная команда."
      logError ("Неизвестная команда: " <> cmd)
      mainMenu appData

askMonth :: IO (Maybe String)
askMonth = do
  putStrLn "Введите месяц 2025 года (формат ГГГГ-ММ):"
  putStr "> "
  month <- getLine
  if validateMonth month
    then pure (Just month)
    else do
      putStrLn "Неверный месяц. Допустимы только значения от 2025-01 до 2025-12."
      logError ("Неверный формат месяца: " <> month)
      pure Nothing

printPayroll :: MonthResult -> IO ()
printPayroll res = do
  putStrLn ""
  putStrLn "Таблица расчета зарплаты:"
  putStrLn (replicate 86 '-')
  putStrLn (printf "%-32s | %8s | %8s | %18s" ("Сотрудник" :: String) ("KPI" :: String) ("Часы" :: String) ("К выплате, руб." :: String))
  putStrLn (replicate 86 '-')
  mapM_
    ( \row ->
        putStrLn $
          printf
            "%-32s | %8.3f | %8.1f | %18.2f"
            (employeeName (pyEmployee row))
            (pyKpiCoeff row)
            (pyWorkedHours row)
            (pyNetSalary row)
    )
    (mrPayrollRows res)
  putStrLn (replicate 86 '-')

printTop :: MonthResult -> IO ()
printTop res = do
  putStrLn ""
  putStrLn "Топ сотрудников по продуктивности:"
  putStrLn (replicate 86 '-')
  putStrLn (printf "%-32s | %14s | %10s" ("Сотрудник" :: String) ("Балл" :: String) ("KPI" :: String))
  putStrLn (replicate 86 '-')
  mapM_
    ( \row ->
        putStrLn $
          printf
            "%-32s | %14.3f | %10.3f"
            (employeeName (prEmployee row))
            (prScore row)
            (prKpiCoeff row)
    )
    (topProductivity 10 (mrProductivityRows res))
  putStrLn (replicate 86 '-')

exportMonth :: String -> MonthResult -> IO ()
exportMonth monthText res = do
  createDirectoryIfMissing True "output"
  let fp = "output/payroll-" <> monthText <> ".csv"
  savePayrollCsv fp (mrPayrollRows res)
  logInfo ("Выгружен отчет: " <> fp)
  putStrLn ("Файл сохранен: " <> fp)

data MonthResult = MonthResult
  { mrProductivityRows :: [ProductivityRow],
    mrPayrollRows :: [PayrollRow]
  }

monthResult :: AppData -> String -> MonthResult
monthResult appData monthText =
  let monthTasks = filterTasksByMonth monthText (adTasks appData)
      productivityRows = buildProductivityRows (adEmployees appData) monthTasks
      payrollRows = buildPayrollRows (adConfig appData) productivityRows
   in MonthResult productivityRows payrollRows
