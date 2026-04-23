module Domain.Types
  ( TaskResult (..),
    TaskType (..),
    TaskRecord (..),
    Employee (..),
    BonusTier (..),
    Config (..),
    ProductivityRow (..),
    PayrollRow (..),
    AppData (..),
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Time (Day)

data TaskResult = Success | Partial | Failed
  deriving (Eq, Show)

data TaskType = TaskType
  { taskTypeId :: String,
    complexityCoeff :: Double
  }
  deriving (Eq, Show)

data TaskRecord = TaskRecord
  { taskEmployeeId :: String,
    taskDate :: Day,
    taskId :: String,
    taskComplexity :: Int,
    taskPlannedHours :: Double,
    taskActualHours :: Double,
    taskResult :: TaskResult
  }
  deriving (Eq, Show)

data Employee = Employee
  { employeeId :: String,
    employeeName :: String,
    employeePosition :: String,
    employeeBaseRate :: Double,
    employeeNorm :: Double
  }
  deriving (Eq, Show)

data BonusTier = BonusTier
  { tierThreshold :: Double,
    tierBonusPercent :: Double
  }
  deriving (Eq, Show)

data Config = Config
  { cfgBaseKpi :: Double,
    cfgBonusScale :: [BonusTier],
    cfgTaxRate :: Double,
    cfgSocialRate :: Double
  }
  deriving (Eq, Show)

data ProductivityRow = ProductivityRow
  { prEmployee :: Employee,
    prScore :: Double,
    prKpiCoeff :: Double,
    prWorkedHours :: Double
  }
  deriving (Eq, Show)

data PayrollRow = PayrollRow
  { pyEmployee :: Employee,
    pyKpiCoeff :: Double,
    pyWorkedHours :: Double,
    pyGrossBase :: Double,
    pyBonusAmount :: Double,
    pyGrossTotal :: Double,
    pyDeductions :: Double,
    pyNetSalary :: Double
  }
  deriving (Eq, Show)

data AppData = AppData
  { adEmployees :: [Employee],
    adTasks :: [TaskRecord],
    adTaskTypes :: M.Map String TaskType,
    adCalendarDaysOff :: S.Set Day,
    adConfig :: Config
  }
  deriving (Eq, Show)
