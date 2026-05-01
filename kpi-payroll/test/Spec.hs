module Main (main) where

import Data.Time (fromGregorian)
import Core.Types
import Productivity.Calculator (productivityForTask)
import Salary.Calculator (buildPayrollRows)
import Test.QuickCheck (Arbitrary (arbitrary), Positive (..), elements, quickCheck)

instance Arbitrary TaskResult where
  arbitrary = elements [Success, Partial, Failed]

prop_productivityNonNegative :: Positive Double -> Positive Double -> Positive Int -> TaskResult -> Bool
prop_productivityNonNegative (Positive planned) (Positive actual) (Positive complexityRaw) result =
  let complexity = 1 + (complexityRaw `mod` 10)
      taskType = TaskType "dev" 1.0
      task =
        TaskRecord
          "e1"
          (fromGregorian 2026 4 24)
          "dev"
          complexity
          planned
          actual
          result
   in productivityForTask taskType task >= 0

prop_salaryNonNegative :: Positive Double -> Positive Double -> Positive Double -> Bool
prop_salaryNonNegative (Positive baseRate) (Positive norm) (Positive hours) =
  let employee = Employee "e1" "Test User" "Engineer" baseRate norm
      prodRow = ProductivityRow employee (norm * 1.1) 1.1 hours
      cfg = Config 1.0 [BonusTier 1.0 0.1] 0.13 0.3 160
      payrollRows = buildPayrollRows cfg [prodRow]
   in case payrollRows of
        [payroll] -> pyNetSalary payroll >= 0 && pyGrossTotal payroll >= 0
        _ -> False

main :: IO ()
main = do
  quickCheck prop_productivityNonNegative
  quickCheck prop_salaryNonNegative
