##---------------------------------------------------------------
## UCR Anomaly Archive
## Univariate series with labeled anomalies
## Recommended use: univariate event detection
##---------------------------------------------------------------

library(dalevents)
library(daltoolbox)
library(harbinger)

## Load series ----------------------
data(ucr)


## Univariate series selection ----------------------
series <- ucr$`005_UCR_Anomaly_DISTORTEDCIMIS44AirTemperature1_4000_5391_5392.RData`$DISTORTEDCIMIS44AirTemperature1_4000_5391_5392
series <- ucr$`023_UCR_Anomaly_DISTORTEDGP711MarkerLFM5z5_5000_8612_8716.RData`$DISTORTEDGP711MarkerLFM5z5_5000_8612_8716
series <- ucr$`003_UCR_Anomaly_DISTORTED3sddb40_35000_46600_46900.RData`$DISTORTED3sddb40_35000_46600_46900
series <- ucr$`010_UCR_Anomaly_DISTORTEDCIMIS44AirTemperature6_4000_6006_6054.RData`$DISTORTEDCIMIS44AirTemperature6_4000_6006_6054
series <- ucr$`034_UCR_Anomaly_DISTORTEDInternalBleeding6_1500_3474_3629.RData`$DISTORTEDInternalBleeding6_1500_3474_3629 #avaliar
series <- ucr$`069_UCR_Anomaly_DISTORTEDinsectEPG5_3200_8500_8501.RData`$DISTORTEDinsectEPG5_3200_8500_8501


plot(as.ts(series))


#Labels
#IDX = 5391_5392
#IDX = 3474_3629
#8500_8501
labels <- data.frame(event = rep(0, length(series)))
labels$event[8500:8501] <- 1

#Sample
#Train: 1-3200
end = 3201
series <- series[end:length(series),]
plot(as.ts(series))

## Preprocessing ----------------------
preproc <- ts_norm_gminmax()
preproc <- fit(preproc, series)
series <- transform(preproc, series)

head(series)
plot(as.ts(series))

## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="UCR",
                         series="AirTemperature1",
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
grf <- har_plot(model, series, detection, labels$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, labels$event)
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
#out_file <- "develop/experiments/result_ucr_AirTemperature1_arima.RData"
#save(detection, file=out_file, compress = TRUE)


## Add new series to experiment ----------------------
series <- ucr$`023_UCR_Anomaly_DISTORTEDGP711MarkerLFM5z5_5000_8612_8716.RData`$DISTORTEDGP711MarkerLFM5z5_5000_8612_8716
plot(as.ts(series))

#Labels
#8612_8716
labels <- data.frame(event = rep(0, length(series)))
labels$event[8612:8716] <- 1


## Preprocessing ----------------------
preproc <- ts_norm_gminmax()
preproc <- fit(preproc, series)
series <- transform(preproc, series)

head(series)
plot(as.ts(series))

#Update experiments
experiment <- rbind(experiment,
                    c(method="hanr_arima",
                      dataset="UCR",
                      series="MarkerLFM5z5",
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
model <- fit(model, series)
t_fit <- Sys.time()-s

#Making detections
s <- Sys.time()
detection <- detect(model, series)
t_det <- Sys.time()-s


# Results analysis ----------------------
#Ploting the results
grf <- har_plot(model, series, detection, labels$event)
plot(grf)
#Evaluating the detection metrics
ev <- evaluate(model, detection$event, labels$event)
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


#Record result
#out_file <- "develop/experiments/result_ucr_MarkerLFM5z5_arima.RData"
#save(detection, file=out_file, compress = TRUE)

#Record overall results
#exp_file <- "develop/experiments/ucr_experiments.RData"
#save(experiment, file=exp_file, compress = TRUE)
