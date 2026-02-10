ETL: UCI 3W Oil Wells

Overview
- Origin: UCI Machine Learning Repository — 3W dataset (undesirable real events in oil wells).
- Goal: Preserve curated 3W series per type and publish package‑ready lists.
- Schema (final per series): `idx` (integer index), `value` and other sensor variables (numeric), `event` (logical), `type` ("Change Point" or empty).

Source Data
- Curated RData files stored in: `ETL/3W/source/oil_3w_Type_*.RData`
- Reference: https://archive.ics.uci.edu/ml/datasets/3W+dataset

Final Data (published in package)
- `harbinger/oil_3w_Type_1.RData`
- `harbinger/oil_3w_Type_2.RData`
- `harbinger/oil_3w_Type_4.RData`
- `harbinger/oil_3w_Type_5.RData`
- `harbinger/oil_3w_Type_6.RData`
- `harbinger/oil_3w_Type_7.RData`
- `harbinger/oil_3w_Type_8.RData`

ETL Code
- `3-load-3w.R`: copies curated RData files to `harbinger/`.

Legacy ETL Notes (from source scripts and docs)
- Raw CSVs were organized by type (`0/1/2/5/6/7/8`) and zipped to `etl/3W/intermediate/zip/<type>/*.RData`.
- A cleaning step removed the first date column and normalized variable names (hyphen to underscore), producing ungrouped series.
- Grouped list artifacts were built under `etl/3W/intermediate/grouped/`.
- Type 2/4/5/8 derived change‑point events from `class` transitions; event labels were converted to logical.
- Type 4 used parquet samples under `etl/3W/intermediate/grouped/parquet/4/` (e.g., `data_3w_tp4_sample.RData`).

Notes
- Variable names are normalized to lower‑case with underscores.
- Change‑point logic follows documented class transitions in originals.
- Background references in this folder: `A realistic and public dataset with rare undesirable real events in oil wells.pdf`,
  `Time series data analysis for automatic flow influx detection during drilling.pdf`.

