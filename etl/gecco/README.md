ETL: GECCO Challenge 2018 (Water Quality)

Overview
- Origin: GECCO 2018 water quality challenge (time series provided via the `EventDetectR` package: `geccoIC2018Train` and `geccoIC2018Test`).
- Goal: Merge train/test, standardize schema, and publish a list of univariate series plus a multivariate entry.
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly"). A `multi` element holds all variables with `event`.

Source Data
- Provided by `EventDetectR` at runtime; no raw files stored in this repo.
- Challenge info: https://www.spotseven.de/gecco/gecco-challenge/

Final Data (published in package)
- `harbinger/gecco.RData`

ETL Code
- `3-load-gecco.R`: loads `EventDetectR`, binds train/test, reshapes each sensor to a series with `idx`, `value`, `event`, adds `type`, and saves `gecco.RData`.

Notes
- Requires the `EventDetectR` package to be installed to access original objects.

