library(united)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(oil_3w_Type_1)

# Select first well from the Type 1 group
data <- oil_3w_Type_1[[1]]

# Choose a sensor (e.g., P_TPT) and plot
series <- data$p_tpt
plot(as.ts(series))

# Establish binary segmentation CPD model
model <- hcp_binseg()

# Fit the CPD model
model <- fit(model, series)

# Produce change-point detections
detection <- detect(model, series)

# Show detected change points
print(detection |> dplyr::filter(event == TRUE))

# Plot detections (blue) vs. labels (red bands)
grf <- har_plot(model, series, detection, data$event)
plot(grf)

# Point-wise evaluation (no tolerance)
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)

ev$accuracy
ev$F1

ev_soft <- evaluate(har_eval_soft(sw = 90), detection$event, data$event)
print(ev_soft$confMatrix)

ev_soft$accuracy
ev_soft$F1
