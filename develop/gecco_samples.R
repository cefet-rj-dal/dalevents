"""
#' @title GECCO — Water Quality Example
#' @description Demonstration script for anomaly detection on water quality
#'     series from the GECCO challenge. Shows multivariate visualization,
#'     univariate selection, training, detection, and evaluation.
#' @references Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection:
#'     A survey. ACM Computing Surveys, 41(3), 1–58.
#' @examples
#' # Run step‑by‑step to reproduce the experiment.
"""
## GECCO — multivariate/univariate anomaly detection example
library(united)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(gecco)

# Plot multivariate subset
plot(as.ts(gecco$multi[, 2:10]))


## Univariate series selection ----------------------
series <- gecco$ph

# Recommended one‑day window with anomalies
series <- series[16500:18000, ]
plot(as.ts(series$value))


## Detection ----------------------
# Define ARIMA‑based detector
model <- hanr_arima()

# Fit the model
model <- fit(model, series$value)

# Produce detections
detection <- detect(model, series$value)


## Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event == TRUE))

# Plot detections against labels
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

# Evaluate metrics
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)

