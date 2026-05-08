module Main (main) where

import Core.Common (renderAppError)
import Core.Types
import DataLoader.Error.Handler (loadAppData, savePayrollCsv)
import Data.List (nub, sort)
import Logging.Logger (logError, logInfo)
import Productivity.Calculator (buildProductivityRows, filterTasksByMonth, topProductivity)
import Salary.Calculator (buildPayrollRows)
import System.Directory (createDirectoryIfMissing)
import Text.Printf (printf)
import Utils.DateUtils (monthPrefix)
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
      month <- askMonth appData
      printPayroll (monthResult appData month)
      logInfo ("Рассчитана зарплата за " <> month)
      mainMenu appData
    "2" -> do
      month <- askMonth appData
      printTop (monthResult appData month)
      logInfo ("Показан топ продуктивности за " <> month)
      mainMenu appData
    "3" -> do
      month <- askMonth appData
      exportMonth month (monthResult appData month)
      mainMenu appData
    "0" -> do
      logInfo "Приложение завершено"
      putStrLn "Работа завершена."
    _ -> do
      putStrLn "Неизвестная команда."
      logError ("Неизвестная команда: " <> cmd)
      mainMenu appData

askMonth :: AppData -> IO String
askMonth appData = do
  putStrLn "Введите месяц (формат ГГГГ-ММ)."
  putStr "> "
  month <- getLine
  if validateMonth month && month `elem` availableMonths appData
    then pure month
    else do
      putStrLn "Неверный месяц. Введите значение из списка доступных месяцев."
      logError ("Неверный формат месяца: " <> month)
      askMonth appData

printPayroll :: MonthResult -> IO ()
printPayroll res = do
  putStrLn ""
  putStrLn "Таблица расчета зарплаты:"
  putStrLn (replicate 132 '-')
  putStrLn (printf "%-28s | %8s | %6s | %6s | %6s | %-15s | %18s" ("Сотрудник" :: String) ("KPI" :: String) ("Часы" :: String) ("Бол." :: String) ("БС" :: String) ("Статус месяца" :: String) ("К выплате, руб." :: String))
  putStrLn (replicate 132 '-')
  mapM_
    ( \row ->
        putStrLn $
          printf
            "%-28s | %8.3f | %6.1f | %6d | %6d | %-15s | %18.2f"
            (employeeName (pyEmployee row))
            (pyKpiCoeff row)
            (pyWorkedHours row)
            (pySickDays row)
            (pyUnpaidDays row)
            (if pyIsPartialMonth row then "неполный месяц" else "полный месяц")
            (pyNetSalary row)
    )
    (mrPayrollRows res)
  putStrLn (replicate 132 '-')

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
      payrollRows = buildPayrollRows (adConfig appData) monthText (adAbsences appData) productivityRows
   in MonthResult productivityRows payrollRows

availableMonths :: AppData -> [String]
availableMonths appData = sort . nub $ map (monthPrefix . taskDate) (adTasks appData)
