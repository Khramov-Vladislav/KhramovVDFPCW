module Salary.Calculator
  ( buildPayrollRows
  )
where

import Core.Types
import Data.List (sortOn)

bonusPercentForKpi :: [BonusTier] -> Double -> Double
bonusPercentForKpi tiers kpi =
  case reverse (sortOn tierThreshold tiers) of
    [] -> 0
    sortedDesc ->
      case filter (\t -> kpi >= tierThreshold t) sortedDesc of
        [] -> 0
        (x : _) -> tierBonusPercent x

performanceMultiplier :: Double -> Double -> Double
performanceMultiplier baseKpi kpi
  | baseKpi <= 0 = 1.0
  | otherwise =
      let normalized = kpi / baseKpi
          raw = 0.85 + 0.5 * normalized
       in max 0.70 (min 1.35 raw)

buildPayrollRows :: Config -> [ProductivityRow] -> [PayrollRow]
buildPayrollRows cfg = map mk
  where
    mk prod =
      let emp = prEmployee prod
          grossBase = employeeBaseRate emp * prWorkedHours prod
          perf = performanceMultiplier (cfgBaseKpi cfg) (prKpiCoeff prod)
          bonusPct = bonusPercentForKpi (cfgBonusScale cfg) (prKpiCoeff prod)
          bonusAmount = grossBase * bonusPct
          grossTotal = grossBase * perf + bonusAmount
          deductions = grossTotal * (cfgTaxRate cfg + cfgSocialRate cfg)
       in PayrollRow
            { pyEmployee = emp,
              pyKpiCoeff = prKpiCoeff prod,
              pyWorkedHours = prWorkedHours prod,
              pyGrossBase = grossBase,
              pyBonusAmount = bonusAmount,
              pyGrossTotal = grossTotal,
              pyDeductions = deductions,
              pyNetSalary = grossTotal - deductions
            }
