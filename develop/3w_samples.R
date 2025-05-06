##---------------------------------------------------------------
## 3W Dataset - Oil wells
## Multivariate series with labeled change points
## Recommended use: multivariate or univariate CPD detection
##---------------------------------------------------------------
library(dalevents)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(oil_3w_Type_1)
#Selecting a well as example
data <- oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`

summary(data)

#Removing empty variables
data$T_JUS_CKGL <- NULL

plot(as.ts(data))


## Preprocessing ----------------------
ts <- data[,1:7]

preproc <- ts_norm_gminmax()
preproc <- fit(preproc, ts)
ts <- transform(preproc, ts)

head(ts)
plot(as.ts(ts))


#Labels adjust ------------------------
labels <- as.data.frame(data$class)
names(labels) <- "class"
plot(as.ts(labels$class))

labels$cpd <- 0
head(labels)

#Initial CP
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
i = cp+1
cp = FALSE
print(i)


for (i in 1018:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)

plot(as.ts(labels$cpd),
     main = "Change Points")


## Univariate series selection ----------------------
series <- ts$P_MON_CKP

plot(as.ts(series))


## Event detection experiment ----------------------
#Experiments results organization
experiment <- data.frame(method="hanr_arima",
                         dataset="3W",
                         series="Type_1_WELL_00001_20140124213136_P_MON_CKP",
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
grf <- har_plot(model, series, detection, labels$cpd)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, labels$cpd)
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


## Final Record -----------------------
#Record result
#out_file <- "develop/experiments/result_Type_1_P_MON_CKP_arima.RData"
#save(detection, file=out_file, compress = TRUE)

#Record overall results
#exp_file <- "develop/experiments/3w_experiments.RData"
#save(experiment, file=exp_file, compress = TRUE)


# SoftEd
ev_soft <- evaluate(har_eval_soft(sw=90), detection$event, as.logical(labels$cpd))
ev_soft$confMatrix
ev_soft$accuracy
ev_soft$precision
ev_soft$recall
ev_soft$F1
