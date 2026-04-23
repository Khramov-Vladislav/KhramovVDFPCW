module Infrastructure.Paths
  ( employeesPath,
    tasksPath,
    taskTypesPath,
    calendarPath,
    configPath,
    outputDir,
    logsDir,
  )
where

employeesPath :: FilePath
employeesPath = "data/employees.csv"

tasksPath :: FilePath
tasksPath = "data/tasks.csv"

taskTypesPath :: FilePath
taskTypesPath = "data/task_types.csv"

calendarPath :: FilePath
calendarPath = "data/calendar.txt"

configPath :: FilePath
configPath = "data/config.json"

outputDir :: FilePath
outputDir = "output"

logsDir :: FilePath
logsDir = "logs"
