module Domain.Payroll
  ( buildPayrollRows,
  )
where

import Data.List (sortOn)
import Domain.Types

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
  | kpi >= baseKpi = kpi
  | otherwise = 0.8 + 0.2 * kpi

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
