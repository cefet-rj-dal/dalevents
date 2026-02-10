"""
#' @title 3W — Label Adjustment and Sanity Checks (Type 1)
#' @description Developer script to explore 3W Type 1 series and reconstruct
#'     change point (CP) labels based on class transitions. Intended for
#'     interactive sanity checks and reproducibility notes.
#' @references Truong, C., Oudre, L., & Vayatis, N. (2020). Selective review of
#'     change point detection methods. Signal Processing, 167, 107299.
#' @keywords internal
"""
library(united)
library(harbinger)
library(daltoolbox)

## 3W Type 1 - Clean and labels adjustment -------------------
data(oil_3w_Type_1)

#Series 1 ------------------------
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$class))

#Labels adjust
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$class)
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

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`)
oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$T_JUS_CKGL <- NULL

summary(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`)
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`))

#Series 2 ------------------------
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$class))


#Labels adjust
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$class)
names(labels) <- "class"
plot(as.ts(labels$class))

labels$cpd <- 0
head(labels)

#Initial CP
i = 1
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
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
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

plot(as.ts(labels))


oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$cpd <- labels$cpd

#Clean NA columns
summary(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`)
oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`))

#Series 3 -------------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$class)
names(labels) <- "class"
labels$cpd <- 0
plot(as.ts(labels))


#Initial CP
i = 1
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
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
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

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`)
oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`))


#Series 4 ------------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$class)
names(labels) <- "class"
labels$cpd <- 0


#Initial CP
i = 1
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
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
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

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`)
oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`))

#Series 5 -----------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$class)
names(labels) <- "class"
labels$cpd <- 0


#Initial CP
i = 1
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
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
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
plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`)
oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`))


#Final check
i = 1
for (i in 1:length(oil_3w_Type_1$Type_1)){
  plot(as.ts(oil_3w_Type_1$Type_1[[i]]),
       main=paste("Series", i))
}


#Record
out_3w <- "data/oil_3w_Type_1.RData"
#save(list = obj_name, file = file, compress = "xz")
save(oil_3w_Type_1, file = out_3w, compress = "xz")




#NAB -------------------------
for (i in 1:length(numenta_realAdExchange$realAdExchange)){
  plot(as.ts(numenta_realAdExchange$realAdExchange[[i]]))
}

for (i in 1:length(nab_realAdExchange)){
  plot(as.ts(nab_realAdExchange[[i]]))
}
data(nab_artificialWithAnomaly)


data(nab_realAWSCloudwatch)

for (i in 1:length(nab_realAWSCloudwatch)){
  plot(as.ts(nab_realAWSCloudwatch[[i]]))
}


for (i in 1:length(nab_realKnownCause)){
  plot(as.ts(nab_realKnownCause[[i]]))
}

for (i in 1:length(nab_realTraffic)){
  plot(as.ts(nab_realTraffic[[i]]))
}

for (i in 1:length(nab_realTweets)){
  plot(as.ts(nab_realTweets[[i]]))
}


#UCR ---------------
#ECG
data(ucr_ecg)
for (i in 1:length(ucr_ecg)){
  plot(as.ts(ucr_ecg[[i]]),
       main=names(ucr_ecg[i]))
}

#NASA
data(ucr_nasa)
for (i in 1:length(ucr_nasa)){
  plot(as.ts(ucr_nasa[[i]]),
       main=names(ucr_nasa[i]))
}


series <- ucr_nasa[[11]]
plot(as.ts(series))


#Event Detection
model <- hanr_arima()
model <- fit(model, series$value)
detection <- detect(model, series$value)

#Plotting the results
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)
print(ev$F1)

ev_soft <- evaluate(har_eval_soft(sw_size = 98), detection$event, series$event)
print(ev_soft$confMatrix)
print(ev_soft$F1)

#INTERNAL BLEEDING
for (i in 1:length(ucr_int_bleeding)){
  plot(as.ts(ucr_int_bleeding[[i]]),
       main=names(ucr_int_bleeding[i]))
}




series <- ucr_int_bleeding[[10]]
plot(as.ts(series$value))


#Event Detection
model <- hanr_arima()
model <- fit(model, series$value)
detection <- detect(model, series$value)

#Plotting the results
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)
print(ev$F1)

ev_soft <- evaluate(har_eval_soft(sw_size = 12), detection$event, series$event)
print(ev_soft$confMatrix)
print(ev_soft$F1)


#POWER DEMAND
data(ucr_power_demand)

for (i in 1:length(ucr_power_demand)){
  plot(as.ts(ucr_power_demand[[i]]),
       main=names(ucr_power_demand[i]))
}
