module Productivity.Calculator
  ( productivityForTask,
    filterTasksByMonth,
    buildProductivityRows,
    topProductivity
  )
where

import Core.Types
import Data.List (sortOn)
import Utils.DateUtils (monthPrefix)

resultFactor :: TaskResult -> Double
resultFactor Success = 1.0
resultFactor Partial = 0.6
resultFactor Failed = 0.0

productivityForTask :: TaskType -> TaskRecord -> Double
productivityForTask taskType taskRec =
  let base = fromIntegral (taskComplexity taskRec) * complexityCoeff taskType
      ratioRaw =
        if taskActualHours taskRec <= 0
          then 2.0
          else taskPlannedHours taskRec / taskActualHours taskRec
      ratio = max 0.5 (min 2.0 ratioRaw)
   in base * ratio * resultFactor (taskResult taskRec)

filterTasksByMonth :: String -> [TaskRecord] -> [TaskRecord]
filterTasksByMonth ym = filter (\t -> monthPrefix (taskDate t) == ym)

buildProductivityRows :: [Employee] -> [TaskRecord] -> [ProductivityRow]
buildProductivityRows employees tasks = map mkRow employees
  where
    tasksByEmployee = foldr collect [] tasks
    collect t acc = (taskEmployeeId t, [t]) : acc
    tasksForEmployee eid = concat [xs | (k, xs) <- tasksByEmployee, k == eid]
    mkRow emp =
      let empTasks = tasksForEmployee (employeeId emp)
          score = sum [productivityForTask (TaskType (taskId t) 1.0) t | t <- empTasks]
          workedHours = sum (map taskActualHours empTasks)
          kpiCoeff = if employeeNorm emp <= 0 then 0 else score / employeeNorm emp
       in ProductivityRow emp score kpiCoeff workedHours

topProductivity :: Int -> [ProductivityRow] -> [ProductivityRow]
topProductivity n rows = take n (reverse (sortOn prScore rows))
