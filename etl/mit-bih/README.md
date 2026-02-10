ETL: MIT‑BIH Arrhythmia Database (Selected Leads)

Overview
- Origin: MIT‑BIH Arrhythmia Database (downloaded RData bundle).
- Goal: Extract selected ECG leads (MLII, V1, V2, V5), standardize schema, derive event markers from annotated beats, and publish package‑ready lists.
- Schema (final per series): `idx` (integer index), `value` (numeric ECG value), `event` (logical, true where annotated beats occur), `seq` (factor beat symbol or NA), `seqlen` (window length, 50).

Source Data
- Bundle origin (manual download): https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData
- Split into per-record files stored in `ETL/mit-bih/source/` as `MIT-BIH-<name>.RData`.

Final Data (published in package)
- `harbinger/mit_bih_MLII.RData`
- `harbinger/mit_bih_V1.RData`
- `harbinger/mit_bih_V2.RData`
- `harbinger/mit_bih_V5.RData`

ETL Code
- `1-extract-split-bundle.R`: splits the original bundle into per-record RData files.
- `3-load-mit-bih.R`: loads split files (or the bundle if present), extracts signals for each lead, converts annotations to `event`, saves lists per lead.

Notes
- Events are set to TRUE where `seq` is non‑NA (i.e., annotated beats); `type` is not used for MIT‑BIH.

