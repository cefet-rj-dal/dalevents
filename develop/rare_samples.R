##---------------------------------------------------------------
## RARE: a labeled dataset for cloud-native memory anomalies
## Multivariate series with labeled anomalies
## Recommended use: multivariate or univariate event detection
## WARNING: Example under construction.
##      This dataset is under our analysis for better
##      organization and suggested use.
##---------------------------------------------------------------
library(dalevents)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(rare)

#RARE dataset content analysis ----------------------
plot(as.ts(rare[,1:10]))
plot(as.ts(rare[,11:20])) #Flat
plot(as.ts(rare[,21:30])) #Flat
plot(as.ts(rare[,31:40]))
plot(as.ts(rare[,41:50])) #Flat
plot(as.ts(rare[,51:60])) #Flat
plot(as.ts(rare[,61:70])) #Flat
plot(as.ts(rare[,71:80])) #Flat
plot(as.ts(rare[,81:90]))
plot(as.ts(rare[,91:100]))

plot(as.ts(rare[,2]))
plot(as.ts(rare[,89]))
plot(as.ts(rare[,90]))


#Series selection ----------------------
series <- rare[2]
plot(as.ts(series))

series$event <- rare$event
names(series) <- c("value", "event")


plot(as.ts(series))


## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="RARE",
                         series="kube_pod_status_ready_0",
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
