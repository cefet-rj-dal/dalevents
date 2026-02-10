library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(tspredit)

## Load series ----------------------
data(ucr_ecg)

# Select a representative series for the experiment
series <- ucr_ecg[[3]]
# Quick visualization of values over time
plot(as.ts(series$value))

# Establish an autoencoder-based detector
# Alternative: uncomment to try ARIMA
# model <- hanr_arima()
model <- han_autoencoder(3, 2, autoenc_ed, num_epochs = 100)

# Fit the model on a training window (subset for speed)
train <- series[1:5000,]
model <- fit(model, train$value)

# Produce anomaly detections for the whole series
detection <- detect(model, series$value)

# Filter detected positives for quick inspection
print(detection |> dplyr::filter(event == TRUE))

# Plot predictions (blue) versus true labels (red bands)
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

# Point-wise evaluation (no temporal tolerance)
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)

# Soft evaluation with temporal tolerance window (sw = 200)
ev_soft <- evaluate(har_eval_soft(sw = 200), detection$event, series$event)
print(ev_soft$confMatrix)

print(ev_soft$accuracy)
print(ev_soft$F1)
