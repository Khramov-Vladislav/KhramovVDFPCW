# kpi-payroll

KPI Payroll - console application in Haskell for:
- productivity accounting by completed tasks,
- integrated KPI calculation for employees,
- payroll calculation using KPI and bonus scale,
- CSV data import/export and user action logging.

## Project structure

- `app/Main.hs` - executable entrypoint.
- `src/Domain/` - pure domain model and calculations.
- `src/Application/` - use-cases orchestration.
- `src/Infrastructure/` - file storage, logging, paths.
- `src/Interface/` - console menu and interaction flow.
- `test/Spec.hs` - QuickCheck properties for key formulas.
- `data/` - input datasets (employees, tasks, config, calendar).
- `output/` - generated payroll reports.
- `logs/` - action log files.

## Run

- `stack run`
- `stack test`
