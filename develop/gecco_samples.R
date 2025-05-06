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
plot(as.ts(gecco$gecco[,1:9]))

#Gecco recommended sample
#One day with anomalies
data <- gecco$gecco
data <- data[16500:18000,]
plot(as.ts(data))


## Preprocessing ----------------------
#data <- na.omit(data)
ts <- data[,1:9]

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

head(ts)
plot(as.ts(ts))


## Univariate series selection ----------------------
series <- ts$ph

plot(as.ts(series))


## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="Gecco",
                         series="ph",
                         elapsed_time_fit=0,
                         elapsed_time_detection=0,
                         accuracy=0,
                         precision=0,
                         recall=0,
                         F1=0)

head(experiment)

#Detection steps
#Establishing arima method
model <- hanr_arima()

#Fitting the model
s <- Sys.time()
model <- fit(model, series)
t_fit <- Sys.time()-s

#Making detections
s <- Sys.time()
detection <- detect(model, series)
t_det <- Sys.time()-s


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))

#Ploting the results
grf <- har_plot(model, series, detection, data$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)


## Experiment update ----------------------
#Time
#Experiment update
experiment$elapsed_time_fit[1] <- as.numeric(t_fit)*60
experiment$elapsed_time_detection[1] <- as.numeric(t_det)*60
#Metrics
experiment$accuracy[1] <- ev$accuracy
experiment$precision[1] <- ev$precision
experiment$recall[1] <- ev$recall
experiment$F1[1] <- ev$F1

print(experiment)

## Record result ----------------------
#out_file <- "develop/experiments/result_gecco_tp_arima.RData"
#save(detection, file=out_file, compress = TRUE)
