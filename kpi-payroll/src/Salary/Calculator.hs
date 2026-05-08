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

absenceFor :: String -> String -> [AbsenceRecord] -> AbsenceRecord
absenceFor monthText empId records =
  case filter (\a -> arEmployeeId a == empId && arMonth a == monthText) records of
    (x : _) -> x
    [] -> AbsenceRecord empId monthText 0 0

buildPayrollRows :: Config -> String -> [AbsenceRecord] -> [ProductivityRow] -> [PayrollRow]
buildPayrollRows cfg monthText absences = map mk
  where
    mk prod =
      let emp = prEmployee prod
          absence = absenceFor monthText (employeeId emp) absences
          sickDays = max 0 (arSickDays absence)
          unpaidDays = max 0 (arUnpaidDays absence)
          workedHours = prWorkedHours prod
          hoursPerDay = max 1 (cfgHoursPerWorkday cfg)
          baselineHours = max 1 (cfgMinMonthlyHours cfg)
          expectedHours = max 1 (baselineHours - fromIntegral (sickDays + unpaidDays) * hoursPerDay)
          isPartialMonth = workedHours < expectedHours * 0.95
          monthCoverage = min 1.0 (workedHours / expectedHours)
          sickPaidHours = fromIntegral sickDays * hoursPerDay * cfgSickDayPayRate cfg
          payableHours = max 0 (workedHours + sickPaidHours - fromIntegral unpaidDays * hoursPerDay)
          grossBase = employeeBaseRate emp * payableHours
          perf = performanceMultiplier (cfgBaseKpi cfg) (prKpiCoeff prod)
          perfEffective = if isPartialMonth then min perf 1.0 else perf
          bonusPct = bonusPercentForKpi (cfgBonusScale cfg) (prKpiCoeff prod)
          bonusAmount = grossBase * bonusPct * monthCoverage
          grossTotal = grossBase * perfEffective + bonusAmount
          deductions = grossTotal * (cfgTaxRate cfg + cfgSocialRate cfg)
       in PayrollRow
            { pyEmployee = emp,
              pyKpiCoeff = prKpiCoeff prod,
              pyWorkedHours = workedHours,
              pySickDays = sickDays,
              pyUnpaidDays = unpaidDays,
              pyIsPartialMonth = isPartialMonth,
              pyGrossBase = grossBase,
              pyBonusAmount = bonusAmount,
              pyGrossTotal = grossTotal,
              pyDeductions = deductions,
              pyNetSalary = grossTotal - deductions
            }
