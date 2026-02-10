"""
#' @title 3W — Oil Wells Change Point Detection Example
#' @description Demonstration script for change point detection (CPD) on a
#'     sensor extracted from the 3W oil wells dataset. Covers loading data,
#'     basic preprocessing, training, detection, visualization, and evaluation.
#' @references Truong, C., Oudre, L., & Vayatis, N. (2020). Selective review of
#'     change point detection methods. Signal Processing, 167, 107299.
#' @examples
#' # Run step‑by‑step to reproduce the experiment.
"""
## 3W — univariate CPD example
library(united)
library(daltoolbox)
library(harbinger)
library(tspredit)
library(daltoolboxdp)


## Load series ----------------------
data(oil_3w_Type_1)
# Select a well as example
data <- oil_3w_Type_1$`WELL-00001_20140124213136`
summary(data)
plot(as.ts(data))


## Preprocessing ----------------------
ts <- data[, 2:8]
#preproc <- ts_norm_gminmax()
#preproc <- fit(preproc, ts)
#ts <- transform(preproc, ts)

head(ts)
plot(as.ts(ts))


## Univariate series selection ----------------------
# Choose a sensor (e.g., P_TPT)
series <- data$p_tpt

plot(as.ts(series))


## Event detection experiment ----------------------
# Define CPD method
model <- hcp_binseg()

# Fit the CPD model
model <- fit(model, series)

# Produce change‑point detections
detection <- detect(model, series)


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event == TRUE))

# Plot detections against labels
grf <- har_plot(model, series, detection, data$event)
plot(grf)

# Evaluate metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)


# Soft evaluation with temporal tolerance
ev_soft <- evaluate(har_eval_soft(sw = 90), detection$event, data$event)
ev_soft$confMatrix
ev_soft$accuracy
ev_soft$precision
ev_soft$recall
ev_soft$F1
