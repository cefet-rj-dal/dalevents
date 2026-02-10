"""
#' @title NAB — Synthetic/Cloud Time Series Example
#' @description Demonstration script for univariate anomaly detection using a
#'     series from the Numenta Anomaly Benchmark (NAB). Covers loading data,
#'     training a baseline detector, visualization, and evaluation.
#' @references Lavin, A., & Ahmad, S. (2015). Evaluating real-time anomaly
#'     detection algorithms — the Numenta Anomaly Benchmark. 2015 IEEE 14th ICMLA.
#' @examples
#' # Run step‑by‑step to reproduce the experiment.
"""
## Numenta Anomaly Benchmark (NAB) — univariate anomaly detection example

library(united)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(nab_artificialWithAnomaly)

# Select first series from the collection
data <- nab_artificialWithAnomaly[[1]]

plot(as.ts(data[, 2:3]), main = names(nab_artificialWithAnomaly[1]))


## Event detection experiment ----------------------
# Define ARIMA-based detector
model <- hanr_arima()

# Fit the model on the value series
model <- fit(model, data$value)

# Produce anomaly detections
detection <- detect(model, data$value)


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event == TRUE))

# Plot detections against labels
grf <- har_plot(model, data$value, detection, data$event)
plot(grf)

# Compute confusion matrix and metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)
