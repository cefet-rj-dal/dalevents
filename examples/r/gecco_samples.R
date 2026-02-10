library(united)
library(daltoolbox)
library(harbinger)
## Load series ----------------------
data(gecco)
# Plot selected multivariate columns
plot(as.ts(gecco$multi[, 2:10]))

## Univariate series selection ----------------------
series <- gecco$ph

# Subset to a recommended one-day window with anomalies
series <- series[16500:18000, ]
plot(as.ts(series$value))

# Establish ARIMA-based detector
model <- hanr_arima()

# Fit the model on the value series
model <- fit(model, series$value)

# Produce anomaly detections
detection <- detect(model, series$value)

# Show detected positives
print(detection |> dplyr::filter(event == TRUE))

# Plot predictions (blue) versus true labels (red bands)
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

# Point-wise evaluation (no temporal tolerance)
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)
