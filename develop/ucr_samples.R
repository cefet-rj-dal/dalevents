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
#Selecting
series <- ucr$`004_UCR_Anomaly_DISTORTEDBIDMC1_2500_5400_5600.RData`
plot(as.ts(series))


#Labels
#IDX = 5400_5600 -> Range defined in dataset documentation
series$event <- 0
series$event[5400:5600] <- 1
names(series) <- c("value", "event")
plot(as.ts(series))

#Sample
#Test: 2500
start = 2500

series <- series[(start+1):nrow(series),]
plot(as.ts(series))

## Preprocessing ----------------------
preproc <- ts_norm_gminmax()
preproc <- fit(preproc, series$value)
series$value <- transform(preproc, series$value)
head(series)

plot(as.ts(series))

## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="UCR",
                         series="BIDMC1",
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
model <- fit(model, series$value)
t_fit <- Sys.time()-s

#Making detections
s <- Sys.time()
detection <- detect(model, series$value)
t_det <- Sys.time()-s


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))

#Ploting the results
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, series$event)
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

#SoftEd Evaluation
s=(5400-5600)*-1
ev_soft <- evaluate(har_eval_soft(sw=s), detection$event, as.logical(series$event))
print(ev_soft$confMatrix)

print(ev_soft$accuracy)
print(ev_soft$F1)

## Record result ----------------------
#out_file <- "develop/experiments/result_ucr_BIDMC1_arima.RData"
#save(detection, file=out_file, compress = TRUE)

#Record overall results
#exp_file <- "develop/experiments/ucr_experiments.RData"
#save(experiment, file=exp_file, compress = TRUE)
