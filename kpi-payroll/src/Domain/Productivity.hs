module Domain.Productivity
  ( productivityForTask,
    filterTasksByMonth,
    buildProductivityRows,
  )
where

import Domain.Types
import qualified Data.Map.Strict as M
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

buildProductivityRows :: M.Map String TaskType -> [Employee] -> [TaskRecord] -> [ProductivityRow]
buildProductivityRows taskTypes employees tasks = map mkRow employees
  where
    tasksByEmployee = M.fromListWith (<>) [(taskEmployeeId t, [t]) | t <- tasks]
    lookupType taskRec = M.findWithDefault (TaskType (taskId taskRec) 1.0) (taskId taskRec) taskTypes
    mkRow emp =
      let empTasks = M.findWithDefault [] (employeeId emp) tasksByEmployee
          score = sum [productivityForTask (lookupType t) t | t <- empTasks]
          workedHours = sum (map taskActualHours empTasks)
          kpiCoeff =
            if employeeNorm emp <= 0
              then 0
              else score / employeeNorm emp
       in ProductivityRow emp score kpiCoeff workedHours
