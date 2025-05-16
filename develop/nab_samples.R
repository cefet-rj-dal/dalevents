##---------------------------------------------------------------
## Numenta Anomaly Benchmark (NAB)  Dataset - Cloud services
## Univariate series with labeled anomalies
## Recommended use: univariate event detection
##---------------------------------------------------------------

library(dalevents)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(nab_artificialWithAnomaly)

#Univerariate use
#Example 1
data <- nab_artificialWithAnomaly[[1]]

plot(as.ts(data[,2:3]),
     main=names(nab_artificialWithAnomaly[1]))


## Event detection experiment ----------------------
#Detection steps
#Establishing arima method
model <- hanr_arima()

#Fitting the model
model <- fit(model, data$value)

#Making detections
detection <- detect(model, data$value)


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))

#Ploting the results
grf <- har_plot(model, data$value, detection, data$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)
