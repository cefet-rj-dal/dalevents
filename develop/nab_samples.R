##---------------------------------------------------------------
## Numenta Anomaly Benchmark (NAB)  Dataset - Cloud services
## Univariate series with labeled anomalies
## Recommended use: univariate event detection
##---------------------------------------------------------------

library(dalevents)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(numenta_artificialWithAnomaly)

#Univerariate use
#Example 1
data <- numenta_artificialWithAnomaly$artificialWithAnomaly$art_daily_flatmidd
plot(as.ts(data))


## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="Numenta",
                         series="artificialWithAnomaly_art_daily_flatmidd",
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
model <- fit(model, data$value)
t_fit <- Sys.time()-s

#Making detections
s <- Sys.time()
detection <- detect(model, data$value)
t_det <- Sys.time()-s


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))

#Ploting the results
grf <- har_plot(model, data$value, detection, data$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)

# Experiment update ----------------------
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
#out_file <- "develop/experiments/result_nab_art_daily_flatmidd_arima.RData"
#save(detection, file=out_file, compress = TRUE)


## Add new series to experiment ----------------------
data(numenta_realAdExchange)
data <- numenta_realAdExchange$realAdExchange$`exchange-3_cpc_resul`
plot(as.ts(data))

#Update experiments
experiment <- rbind(experiment,
                    c(method="hanr_arima",
                      dataset="Numenta",
                      series="realAdExchange_exchange-3_cpc_resul",
                      elapsed_time_fit=0,
                      elapsed_time_detection=0,
                      accuracy=0,
                      precision=0,
                      recall=0,
                      F1=0))

print(experiment)

#Repeat detection steps
#   Repeated steps for didactic reasons
#   WARNING: In real experimental situations, variable selection and repetition
#   of detection steps should be encapsulated in a loop or a function

#Detection steps
#Establishing arima method
model <- hanr_arima()

#Fitting the model
s <- Sys.time()
model <- fit(model, data$event)
t_fit <- Sys.time()-s

#Making detections
s <- Sys.time()
detection <- detect(model, data$event)
t_det <- Sys.time()-s

# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))
#Ploting the results
grf <- har_plot(model, data$event, detection, data$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)


#Experiment update
#Time
experiment$elapsed_time_fit[2] <- as.numeric(t_fit)*60
experiment$elapsed_time_detection[2] <- as.numeric(t_det)*60
#Metrics
experiment$accuracy[2] <- ev$accuracy
experiment$precision[2] <- ev$precision
experiment$recall[2] <- ev$recall
experiment$F1[2] <- ev$F1

print(experiment)

## Final Record -----------------------
#Record result
#out_file <- "develop/experiments/result_nab_realAdExchange_exchange-3_cpc_resul.RData"
#save(detection, file=out_file, compress = TRUE)

#Record overall results
#exp_file <- "develop/experiments/nab_experiments.RData"
#save(experiment, file=exp_file, compress = TRUE)
