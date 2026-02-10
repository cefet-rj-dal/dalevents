---
title: "UniTED Example Notebooks"
output: github_document
---

This folder collects example R Markdown notebooks that demonstrate how to load
datasets from the UniTED package, run lightweight detection workflows, and
interpret results. Each notebook is self‑contained and can be knit to HTML for
sharing.

## How to Run

- Open any notebook below in RStudio (or your IDE of choice).
- Ensure the UniTED package and listed dependencies are installed.
- Knit to HTML (or run code chunks interactively) to reproduce the results.

## Notebooks

- UCR ECG — Univariate Anomaly Detection  
  File: `examples/ucr_ecg_samples.md`  
  Purpose: Demonstrates anomaly detection on ECG series from the UCR Anomaly
  Archive. Loads the dataset, visualizes a selected series, trains a baseline
  detector (e.g., autoencoder/ARIMA), and evaluates with both point‑wise and
  soft (tolerant) metrics.

- NAB — Cloud and Synthetic Time Series  
  File: `examples/nab_samples.md`  
  Purpose: Shows univariate anomaly detection on series from the Numenta
  Anomaly Benchmark (NAB), including visualization of detected events and
  confusion‑matrix‑based evaluation.

- GECCO — Water Quality Challenge  
  File: `examples/gecco_samples.md`  
  Purpose: Illustrates anomaly detection on multivariate environmental time
  series. Visualizes multiple sensors, selects a univariate stream for a focused
  experiment, and evaluates a baseline detector.

- 3W — Oil Wells Change Point Detection  
  File: `examples/3w_samples.md`  
  Purpose: Demonstrates change point detection (CPD) on a sensor extracted from
  the 3W oil wells dataset, including plotting of predictions vs. labeled change
  points and both point‑wise and soft evaluation.

## References

- Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection: A survey.
  ACM Computing Surveys, 41(3), 1–58.
- Truong, C., Oudre, L., & Vayatis, N. (2020). Selective review of change point
  detection methods. Signal Processing, 167, 107299.
- Lavin, A., & Ahmad, S. (2015). Evaluating real‑time anomaly detection
  algorithms — the Numenta Anomaly Benchmark. 2015 IEEE 14th ICMLA.

