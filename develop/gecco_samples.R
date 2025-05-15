##---------------------------------------------------------------
## Gecco Challenge Dataset - Water Quality
## Multivariate series with labeled anomalies
## Recommended use: multivariate or univariate event detection
##---------------------------------------------------------------
library(dalevents)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(gecco)

#Plot multivariate series
plot(as.ts(gecco$multi[,2:10]))


## Univariate series selection ----------------------
series <- gecco$ph

#Gecco recommended sample: One day with anomalies
series <- series[16500:18000,]
plot(as.ts(series$value))


## Detection ----------------------
#Establishing arima method
model <- hanr_arima()

#Fitting the model
model <- fit(model, series$value)

#Making detections
detection <- detect(model, series$value)


## Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))

#Ploting the results
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)

