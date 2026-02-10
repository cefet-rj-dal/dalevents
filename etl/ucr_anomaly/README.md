ETL: UCR Time Series Anomaly Archive

Overview
- Origin: UCR Time Series Anomaly Archive — labeled anomalous intervals across multiple domains.
- Goal: Optionally zip original TXT files, prepare grouped index of series, and build package‑ready lists for selected domains (ECG, NASA, Internal Bleeding, Power Demand).
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly").

Source Data
- Labeled RData (per domain): `etl/ucr_anomaly/source/ucr_ecg.RData`, `ucr_nasa.RData`, `ucr_int_bleeding.RData`, `ucr_power_demand.RData`
- (Optional) Original TXT: `etl/ucr_anomaly/original/` (if present; used by zipping step)
- Archive info: https://www.cs.ucr.edu/~eamonn/discords/ — and catalog mirrors (see Papers With Code dataset page)

Intermediate Artifacts
- Zipped originals: `etl/ucr_anomaly/intermediate/zip/*.RData`
- Grouped index of all zipped series: `etl/ucr_anomaly/intermediate/grouped/ucr.RData`

Final Data (published in package)
- `harbinger/ucr_ecg.RData`
- `harbinger/ucr_nasa.RData`
- `harbinger/ucr_int_bleeding.RData`
- `harbinger/ucr_power_demand.RData`

ETL Code
- `1-extract-ucr.R`: zip original TXT files to RData and build grouped index.
- `2-transform-create-labels.R`: helper to build event labels from metadata.
- `2-transform-labels-ucr.R`: generate labeled `ucr.RData` and `ucr_label_interval.RData`.
- `2-transform-ucr-ecg.R`: select ECG series into `etl/ucr_anomaly/source/ucr_ecg.RData`.
- `2-transform-ucr-internal-bleeding.R`: select Internal Bleeding series into `etl/ucr_anomaly/source/ucr_int_bleeding.RData`.
- `2-transform-ucr-nasa.R`: select NASA series into `etl/ucr_anomaly/source/ucr_nasa.RData`.
- `2-transform-ucr-power-demand.R`: select Power Demand series into `etl/ucr_anomaly/source/ucr_power_demand.RData`.
- `3-load-ucr-ecg.R`: build final ECG list from labeled source RData.
- `3-load-ucr-internal-bleeding.R`: build final Internal Bleeding list.
- `3-load-ucr-nasa.R`: build final NASA list.
- `3-load-ucr-power-demand.R`: build final Power Demand list.

Notes
- Each final list entry has `idx`, `value`, `event`, and `type` fields.

