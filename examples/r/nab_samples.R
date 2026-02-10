library(united)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(nab_artificialWithAnomaly)

# Select the first series from the collection
data <- nab_artificialWithAnomaly[[1]]

# Visualize value and label columns
plot(as.ts(data[, 2:3]),
     main = names(nab_artificialWithAnomaly[1]))

# Establish ARIMA-based detector
model <- hanr_arima()

# Fit the model on the value series
model <- fit(model, data$value)

# Produce anomaly detections
detection <- detect(model, data$value)

# Show detected positives only
print(detection |> dplyr::filter(event == TRUE))

# Plot predictions (blue) over true labels (red bands)
grf <- har_plot(model, data$value, detection, data$event)
plot(grf)

# Point-wise evaluation (no temporal tolerance)
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)
