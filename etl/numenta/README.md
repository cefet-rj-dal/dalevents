ETL: Numenta Anomaly Benchmark (NAB)

Overview
- Origin: NAB (Numenta) — real and synthetic time series with labeled anomalies.
- Goal: Zip original CSVs, attach labels, prepare ungrouped source series, and publish package‑ready lists by collection.
- Schema (final per series): `idx` (integer index), `value` (numeric), `event` (logical), `type` ("anomaly").

Source Data
- Original CSVs (if present): `etl/numenta/original/<group>/`
- Labels (RDS): `etl/numenta/labels/<group>/`
- Labeled ungrouped series (RData): `etl/numenta/source/<group>/*.RData`
- Groups: `artificialWithAnomaly`, `realAdExchange`, `realAWSCloudwatch`, `realKnownCause`, `realTraffic`, `realTweets`
- NAB reference: https://github.com/numenta/NAB

Intermediate Artifacts
- Zipped raw CSVs: `etl/numenta/intermediate/zip/<group>/*.RData`
- Grouped lists (by group): `etl/numenta/intermediate/grouped/numenta_<group>.RData`
- All labeled groups combined: `etl/numenta/intermediate/grouped/numenta_grp_all.RData`

Final Data (published in package)
- `harbinger/nab_artificialWithAnomaly.RData`
- `harbinger/nab_realAdExchange.RData`
- `harbinger/nab_realAWSCloudwatch.RData`
- `harbinger/nab_realKnownCause.RData`
- `harbinger/nab_realTraffic.RData`
- `harbinger/nab_realTweets.RData`

ETL Code
- `1-extract-numenta.R`: zip originals, attach labels, build ungrouped labeled series and grouped lists.
- `3-load-artificialWithAnomaly.R`: build final `harbinger/nab_artificialWithAnomaly.RData` from source RData.
- `3-load-realAdExchange.R`: build final list.
- `3-load-realAWSCloudwatch.R`: build final list.
- `3-load-realKnownCause.R`: build final list.
- `3-load-realTraffic.R`: build final list.
- `3-load-realTweets.R`: build final list.

Notes
- `event` is logical; `type` marks anomaly. All series include an integer `idx` column.

