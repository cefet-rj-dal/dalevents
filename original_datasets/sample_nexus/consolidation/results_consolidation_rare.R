#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")


# RARE --------------------------------------------------------------------
load("~/janio/harbinger/dev/rare_sample.RData")

## rare_243_mem9 -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load("exp_rare_243_mem9.RData")

#Run the next 4 lines only if decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_243_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_243_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_243_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_243_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_243_mem9.RData")

#Resume
rare_243_mem9_cons <- list()


#FBIAD =====================================
ev_rare_243_mem9_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_mem9_fbiad[[j]] <- evaluate(result_rare_fbiad_243_mem9[[j]]$detector,
                                          result_rare_fbiad_243_mem9[[j]]$detection$event,
                                          rare_sample$event)
}


ev_soft_rare_243_mem9_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_mem9_fbiad[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_fbiad_243_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_mem9_fbiad[[1]]
names(ev_soft_rare_243_mem9_fbiad) <- names(rare_sample)[1:20]


#F1
f1_rare_243_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_fbiad)) {
  print(paste("F1", j, ":"))
  print(ev_rare_243_mem9_fbiad[[j]]$F1)
  f1_rare_243_mem9_fbiad <- append(f1_rare_243_mem9_fbiad, ev_rare_243_mem9_fbiad[[j]]$F1)
}


f1_rare_243_mem9_fbiad

rare_243_mem9_cons$f1 <- data.frame(method = "FBIAD", value = mean(f1_rare_243_mem9_fbiad, na.rm = TRUE))

rare_243_mem9_cons


f1_soft_rare_243_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_fbiad)) {
  f1_soft_rare_243_mem9_fbiad <- tryCatch({
    append(f1_soft_rare_243_mem9_fbiad, ev_soft_rare_243_mem9_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_mem9_fbiad, NA))
  })
}

rare_243_mem9_cons$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_rare_243_mem9_fbiad, na.rm = TRUE))

rare_243_mem9_cons


#Accuracy
acc_rare_243_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_fbiad)) {
  print(paste("Accuracy", j, ":"))
  print(ev_rare_243_mem9_fbiad[[j]]$accuracy)
  acc_rare_243_mem9_fbiad <- append(acc_rare_243_mem9_fbiad, ev_rare_243_mem9_fbiad[[j]]$accuracy)
}


acc_rare_243_mem9_fbiad
rare_243_mem9_cons$acc <- data.frame(method = "FBIAD", value = mean(acc_rare_243_mem9_fbiad, na.rm = TRUE))
rare_243_mem9_cons

#Time

result_rare_fbiad_243_mem9[[1]]$time
length(diff(result_rare_fbiad_243_mem9[[1]]$time))


time_per_batch_rare_243_mem9_fbiad <- c()
j = 1

for (j in 1:20) {
  time_per_batch_rare_243_mem9_fbiad <- append(
    time_per_batch_rare_243_mem9_fbiad,
    diff(result_rare_fbiad_243_mem9[[j]]$time))
}

time_per_batch_rare_243_mem9_fbiad
rare_243_mem9_cons$time_per_batch <- data.frame(method = "FBIAD",
                                                value = mean(time_per_batch_rare_243_mem9_fbiad, na.rm = TRUE))
rare_243_mem9_cons

#arima ##===============================
ev_rare_243_mem9_arima <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_mem9_arima[[j]] <- evaluate(result_rare_arima_243_mem9[[j]]$detector,
                                          result_rare_arima_243_mem9[[j]]$detection$event,
                                          rare_sample$event)
}


ev_rare_243_mem9_arima[[1]]
names(ev_rare_243_mem9_arima) <- names(rare_sample)[1:20]


ev_soft_rare_243_mem9_arima <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_mem9_arima[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_arima_243_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_mem9_arima[[1]]
names(ev_soft_rare_243_mem9_arima) <- names(rare_sample)[1:20]


#F1
f1_rare_243_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_arima)) {
  f1_rare_243_mem9_arima <- append(f1_rare_243_mem9_arima, ev_rare_243_mem9_arima[[j]]$F1)
}


f1_rare_243_mem9_arima

rare_243_mem9_cons$f1 <- rbind(rare_243_mem9_cons$f1, c(method = "ARIMA", value = mean(f1_rare_243_mem9_arima, na.rm = TRUE)))
rare_243_mem9_cons


f1_soft_rare_243_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_arima)) {
  f1_soft_rare_243_mem9_arima <- tryCatch({
    append(f1_soft_rare_243_mem9_arima, ev_soft_rare_243_mem9_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_mem9_arima, NA))
  })
}

rare_243_mem9_cons$f1_soft <- rbind(rare_243_mem9_cons$f1_soft, c(method = "ARIMA", value = mean(f1_soft_rare_243_mem9_arima, na.rm = TRUE)))

rare_243_mem9_cons

#Accuracy
acc_rare_243_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_arima)) {
  print(paste("Accuracy", j, ":"))
  print(ev_rare_243_mem9_arima[[j]]$accuracy)
  acc_rare_243_mem9_arima <- append(acc_rare_243_mem9_arima, ev_rare_243_mem9_arima[[j]]$accuracy)
}


acc_rare_243_mem9_arima
rare_243_mem9_cons$acc <- rbind(rare_243_mem9_cons$acc, c(method = "ARIMA", value = mean(acc_rare_243_mem9_arima, na.rm = TRUE)))
rare_243_mem9_cons

#Time
time_per_batch_rare_243_mem9_arima <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_mem9_arima <- append(
    time_per_batch_rare_243_mem9_arima,
    diff(result_rare_arima_243_mem9[[j]]$time))
}

time_per_batch_rare_243_mem9_arima

rare_243_mem9_cons$time_per_batch <- rbind(rare_243_mem9_cons$time_per_batch,
                                           c(method = "ARIMA",
                                             value = mean(time_per_batch_rare_243_mem9_arima, na.rm = TRUE)))
rare_243_mem9_cons




#garch ##===============================
ev_rare_243_mem9_garch <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_mem9_garch[[j]] <- tryCatch({
    evaluate(result_rare_garch_243_mem9[[j]]$detector,
             result_rare_garch_243_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_mem9_garch

names(ev_rare_243_mem9_garch) <- names(rare_sample)[1:20]


ev_soft_rare_243_mem9_garch <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_mem9_garch[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_garch_243_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_mem9_garch[[1]]
names(ev_soft_rare_243_mem9_garch) <- names(rare_sample)[1:20]

#F1
f1_rare_243_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_garch)) {
  f1_rare_243_mem9_garch <- tryCatch({
    append(f1_rare_243_mem9_garch, ev_rare_243_mem9_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_mem9_garch,NA))
  }) 
}


f1_rare_243_mem9_garch


rare_243_mem9_cons$f1 <- rbind(rare_243_mem9_cons$f1, c(method = "GARCH", value = mean(f1_rare_243_mem9_garch, na.rm = TRUE)))

rare_243_mem9_cons


f1_soft_rare_243_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_garch)) {
  f1_soft_rare_243_mem9_garch <- tryCatch({
    append(f1_soft_rare_243_mem9_garch, ev_soft_rare_243_mem9_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_mem9_garch, NA))
  })
}

rare_243_mem9_cons$f1_soft <- rbind(rare_243_mem9_cons$f1_soft, c(method = "GARCH", value = mean(f1_soft_rare_243_mem9_garch, na.rm = TRUE)))

rare_243_mem9_cons


#Accuracy
acc_rare_243_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_garch)) {
  acc_rare_243_mem9_garch <- tryCatch({
    append(acc_rare_243_mem9_garch, ev_rare_243_mem9_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_mem9_garch,NA))
  })
}


acc_rare_243_mem9_garch
rare_243_mem9_cons$acc <- rbind(rare_243_mem9_cons$acc, c(method = "GARCH", value = mean(acc_rare_243_mem9_garch, na.rm = TRUE)))
rare_243_mem9_cons

#Time
time_per_batch_rare_243_mem9_garch <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_mem9_garch <- tryCatch({
    append(time_per_batch_rare_243_mem9_garch, diff(result_rare_garch_243_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_mem9_garch, NA))
  })
}

time_per_batch_rare_243_mem9_garch

rare_243_mem9_cons$time_per_batch <- rbind(rare_243_mem9_cons$time_per_batch,
                                           c(method = "GARCH",
                                             value = mean(time_per_batch_rare_243_mem9_garch, na.rm = TRUE)))
rare_243_mem9_cons


#cf_lr ##===============================
ev_rare_243_mem9_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_mem9_cf_lr[[j]] <- tryCatch({
    evaluate(result_rare_cf_lr_243_mem9[[j]]$detector,
             result_rare_cf_lr_243_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_mem9_cf_lr

names(ev_rare_243_mem9_cf_lr) <- names(rare_sample)[1:20]


ev_soft_rare_243_mem9_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_mem9_cf_lr[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_cf_lr_243_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_mem9_cf_lr[[1]]
names(ev_soft_rare_243_mem9_cf_lr) <- names(rare_sample)[1:20]


#F1
f1_rare_243_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_cf_lr)) {
  f1_rare_243_mem9_cf_lr <- tryCatch({
    append(f1_rare_243_mem9_cf_lr, ev_rare_243_mem9_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_mem9_cf_lr,NA))
  }) 
}


f1_rare_243_mem9_cf_lr


rare_243_mem9_cons$f1 <- rbind(rare_243_mem9_cons$f1, c(method = "CF_LR", value = mean(f1_rare_243_mem9_cf_lr, na.rm = TRUE)))

rare_243_mem9_cons


f1_soft_rare_243_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_cf_lr)) {
  f1_soft_rare_243_mem9_cf_lr <- tryCatch({
    append(f1_soft_rare_243_mem9_cf_lr, ev_soft_rare_243_mem9_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_mem9_cf_lr, NA))
  })
}

rare_243_mem9_cons$f1_soft <- rbind(rare_243_mem9_cons$f1_soft, c(method = "CF_LR", value = mean(f1_soft_rare_243_mem9_cf_lr, na.rm = TRUE)))

rare_243_mem9_cons

#Accuracy
acc_rare_243_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_cf_lr)) {
  acc_rare_243_mem9_cf_lr <- tryCatch({
    append(acc_rare_243_mem9_cf_lr, ev_rare_243_mem9_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_mem9_cf_lr,NA))
  })
}


acc_rare_243_mem9_cf_lr
rare_243_mem9_cons$acc <- rbind(rare_243_mem9_cons$acc, c(method = "CF_LR", value = mean(acc_rare_243_mem9_cf_lr, na.rm = TRUE)))
rare_243_mem9_cons

#Time
time_per_batch_rare_243_mem9_cf_lr <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_mem9_cf_lr <- tryCatch({
    append(time_per_batch_rare_243_mem9_cf_lr, diff(result_rare_cf_lr_243_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_mem9_cf_lr, NA))
  })
}

time_per_batch_rare_243_mem9_cf_lr

rare_243_mem9_cons$time_per_batch <- rbind(rare_243_mem9_cons$time_per_batch,
                                           c(method = "CF_LR",
                                             value = mean(time_per_batch_rare_243_mem9_cf_lr, na.rm = TRUE)))
rare_243_mem9_cons


#ml_lstm ##===============================
ev_rare_243_mem9_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_mem9_ml_lstm[[j]] <- tryCatch({
    evaluate(result_rare_ml_lstm_243_mem9[[j]]$detector,
             result_rare_ml_lstm_243_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_mem9_ml_lstm

names(ev_rare_243_mem9_ml_lstm) <- names(rare_sample)[1:20]


ev_soft_rare_243_mem9_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_mem9_ml_lstm[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_ml_lstm_243_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_mem9_ml_lstm[[1]]
names(ev_soft_rare_243_mem9_ml_lstm) <- names(rare_sample)[1:20]


#F1
f1_rare_243_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_ml_lstm)) {
  f1_rare_243_mem9_ml_lstm <- tryCatch({
    append(f1_rare_243_mem9_ml_lstm, ev_rare_243_mem9_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_mem9_ml_lstm,NA))
  }) 
}


f1_rare_243_mem9_ml_lstm

rare_243_mem9_cons$f1 <- rbind(rare_243_mem9_cons$f1, c(method = "ML_LSTM", value = mean(f1_rare_243_mem9_ml_lstm, na.rm = TRUE)))
rare_243_mem9_cons


f1_soft_rare_243_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_ml_lstm)) {
  f1_soft_rare_243_mem9_ml_lstm <- tryCatch({
    append(f1_soft_rare_243_mem9_ml_lstm, ev_soft_rare_243_mem9_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_mem9_ml_lstm, NA))
  })
}

rare_243_mem9_cons$f1_soft <- rbind(rare_243_mem9_cons$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_rare_243_mem9_ml_lstm, na.rm = TRUE)))
rare_243_mem9_cons


#Accuracy
acc_rare_243_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_mem9_ml_lstm)) {
  acc_rare_243_mem9_ml_lstm <- tryCatch({
    append(acc_rare_243_mem9_ml_lstm, ev_rare_243_mem9_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_mem9_ml_lstm,NA))
  })
}


acc_rare_243_mem9_ml_lstm
rare_243_mem9_cons$acc <- rbind(rare_243_mem9_cons$acc, c(method = "ML_LSTM", value = mean(acc_rare_243_mem9_ml_lstm, na.rm = TRUE)))
rare_243_mem9_cons

#Time
time_per_batch_rare_243_mem9_ml_lstm <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_mem9_ml_lstm <- tryCatch({
    append(time_per_batch_rare_243_mem9_ml_lstm, diff(result_rare_ml_lstm_243_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_mem9_ml_lstm, NA))
  })
}

time_per_batch_rare_243_mem9_ml_lstm

rare_243_mem9_cons$time_per_batch <- rbind(rare_243_mem9_cons$time_per_batch,
                                           c(method = "ML_LSTM",
                                             value = mean(time_per_batch_rare_243_mem9_ml_lstm, na.rm = TRUE)))
rare_243_mem9_cons

##===========================
#Save final results
save(rare_243_mem9_cons, file = "~/janio/harbinger/dev/results/rare_243_mem9_cons.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/rare_243_mem9_cons.RData")
rare_243_mem9_cons

## rare_243 -----------------------------------------------------------
##ADJUST BEFORE RUNNING
## rare_243 -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load("exp_rare_243.RData")

#Run the next 4 lines only if decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_243.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_243.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_243.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_243.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_243.RData")

#Resume ====
rare_243_cons <- list()


ev_rare_243_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_fbiad[[j]] <- evaluate(result_rare_fbiad_243[[j]]$detector,
                                     result_rare_fbiad_243[[j]]$detection$event,
                                     rare_sample$event)
}


ev_rare_243_fbiad[[1]]
names(ev_rare_243_fbiad) <- names(rare_sample)[1:20]


ev_soft_rare_243_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_fbiad[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_fbiad_243[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_fbiad[[1]]
names(ev_soft_rare_243_fbiad) <- names(rare_sample)[1:20]


#F1
f1_rare_243_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_fbiad)) {
  f1_rare_243_fbiad <- tryCatch({
    append(f1_rare_243_fbiad, ev_rare_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_fbiad, NA))
  })
}


f1_rare_243_fbiad

rare_243_cons$f1 <- data.frame(method = "FBIAD", value = mean(f1_rare_243_fbiad, na.rm = TRUE))
rare_243_cons


f1_soft_rare_243_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_fbiad)) {
  f1_soft_rare_243_fbiad <- tryCatch({
    append(f1_soft_rare_243_fbiad, ev_soft_rare_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_fbiad, NA))
  })
}

rare_243_cons$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_rare_243_fbiad, na.rm = TRUE))

rare_243_cons

#Accuracy
acc_rare_243_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_fbiad)) {
  acc_rare_243_fbiad <- tryCatch({
    append(acc_rare_243_fbiad, ev_rare_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_fbiad, NA))
  })
}


acc_rare_243_fbiad
rare_243_cons$acc <- data.frame(method = "FBIAD", value = mean(acc_rare_243_fbiad, na.rm = TRUE))
rare_243_cons

#Time
result_rare_fbiad_243[[1]]$time
length(diff(result_rare_fbiad_243[[1]]$time))


time_per_batch_rare_243_fbiad <- c()
j = 1

for (j in 1:20) {
  time_per_batch_rare_243_fbiad <- tryCatch({
    append(
      time_per_batch_rare_243_fbiad,
      diff(result_rare_fbiad_243[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_fbiad, NA))
  })
}

time_per_batch_rare_243_fbiad
rare_243_cons$time_per_batch <- data.frame(method = "FBIAD",
                                           value = mean(time_per_batch_rare_243_fbiad, na.rm = TRUE))
rare_243_cons

#arima
## arima ===============================
ev_rare_243_arima <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_arima[[j]] <- evaluate(result_rare_arima_243[[j]]$detector,
                                     result_rare_arima_243[[j]]$detection$event,
                                     rare_sample$event)
}


ev_rare_243_arima[[1]]
names(ev_rare_243_arima) <- names(rare_sample)[1:20]


ev_soft_rare_243_arima <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_arima[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_arima_243[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_arima[[1]]
names(ev_soft_rare_243_arima) <- names(rare_sample)[1:20]



#F1
f1_rare_243_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_arima)) {
  f1_rare_243_arima <- tryCatch({
    append(f1_rare_243_arima, ev_rare_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_arima, NA))
  })
}


f1_rare_243_arima


rare_243_cons$f1 <- rbind(rare_243_cons$f1, c(method = "ARIMA", value = mean(f1_rare_243_arima, na.rm = TRUE)))

rare_243_cons


f1_soft_rare_243_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_arima)) {
  f1_soft_rare_243_arima <- tryCatch({
    append(f1_soft_rare_243_arima, ev_soft_rare_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_arima, NA))
  })
}

rare_243_cons$f1_soft <- rbind(rare_243_cons$f1_soft, c(method = "ARIMA", value = mean(f1_soft_rare_243_arima, na.rm = TRUE)))

rare_243_cons


#Accuracy
acc_rare_243_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_arima)) {
  acc_rare_243_arima <- tryCatch({
    append(acc_rare_243_arima, ev_rare_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_arima, NA))
  })
}


acc_rare_243_arima
rare_243_cons$acc <- rbind(rare_243_cons$acc, c(method = "ARIMA", value = mean(acc_rare_243_arima, na.rm = TRUE)))
rare_243_cons

#Time
time_per_batch_rare_243_arima <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_arima <- tryCatch({
    append(
      time_per_batch_rare_243_arima,
      diff(result_rare_arima_243[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(
      time_per_batch_rare_243_arima, NA))
  })
}

time_per_batch_rare_243_arima

rare_243_cons$time_per_batch <- rbind(rare_243_cons$time_per_batch,
                                      c(method = "ARIMA",
                                        value = mean(time_per_batch_rare_243_arima, na.rm = TRUE)))
rare_243_cons




#garch
## garch ===============================
ev_rare_243_garch <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_garch[[j]] <- tryCatch({
    evaluate(result_rare_garch_243[[j]]$detector,
             result_rare_garch_243[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_garch

names(ev_rare_243_garch) <- names(rare_sample)[1:20]


ev_soft_rare_243_garch <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_garch[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_garch_243[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_garch[[1]]
names(ev_soft_rare_243_garch) <- names(rare_sample)[1:20]

#F1
f1_rare_243_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_garch)) {
  f1_rare_243_garch <- tryCatch({
    append(f1_rare_243_garch, ev_rare_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_garch,NA))
  }) 
}


f1_rare_243_garch


rare_243_cons$f1 <- rbind(rare_243_cons$f1, c(method = "GARCH", value = mean(f1_rare_243_garch, na.rm = TRUE)))

rare_243_cons

f1_soft_rare_243_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_garch)) {
  f1_soft_rare_243_garch <- tryCatch({
    append(f1_soft_rare_243_garch, ev_soft_rare_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_garch, NA))
  })
}

rare_243_cons$f1_soft <- rbind(rare_243_cons$f1_soft, c(method = "GARCH", value = mean(f1_soft_rare_243_garch, na.rm = TRUE)))

rare_243_cons

#Accuracy
acc_rare_243_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_garch)) {
  acc_rare_243_garch <- tryCatch({
    append(acc_rare_243_garch, ev_rare_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_garch,NA))
  })
}


acc_rare_243_garch
rare_243_cons$acc <- rbind(rare_243_cons$acc, c(method = "GARCH", value = mean(acc_rare_243_garch, na.rm = TRUE)))
rare_243_cons

#Time
time_per_batch_rare_243_garch <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_garch <- tryCatch({
    append(time_per_batch_rare_243_garch, diff(result_rare_garch_243[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_garch, NA))
  })
}

time_per_batch_rare_243_garch

rare_243_cons$time_per_batch <- rbind(rare_243_cons$time_per_batch,
                                      c(method = "GARCH",
                                        value = mean(time_per_batch_rare_243_garch, na.rm = TRUE)))
rare_243_cons


#cf_lr
## cf_lr ===============================
ev_rare_243_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_cf_lr[[j]] <- tryCatch({
    evaluate(result_rare_cf_lr_243[[j]]$detector,
             result_rare_cf_lr_243[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_cf_lr

names(ev_rare_243_cf_lr) <- names(rare_sample)[1:20]


ev_soft_rare_243_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_cf_lr[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_cf_lr_243[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_cf_lr[[1]]
names(ev_soft_rare_243_cf_lr) <- names(rare_sample)[1:20]

#F1
f1_rare_243_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_cf_lr)) {
  f1_rare_243_cf_lr <- tryCatch({
    append(f1_rare_243_cf_lr, ev_rare_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_cf_lr,NA))
  }) 
}


f1_rare_243_cf_lr


rare_243_cons$f1 <- rbind(rare_243_cons$f1, c(method = "CF_LR", value = mean(f1_rare_243_cf_lr, na.rm = TRUE)))

rare_243_cons


f1_soft_rare_243_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_cf_lr)) {
  f1_soft_rare_243_cf_lr <- tryCatch({
    append(f1_soft_rare_243_cf_lr, ev_soft_rare_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_cf_lr, NA))
  })
}

rare_243_cons$f1_soft <- rbind(rare_243_cons$f1_soft, c(method = "CF_LR", value = mean(f1_soft_rare_243_cf_lr, na.rm = TRUE)))

rare_243_cons

#Accuracy
acc_rare_243_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_cf_lr)) {
  acc_rare_243_cf_lr <- tryCatch({
    append(acc_rare_243_cf_lr, ev_rare_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_cf_lr,NA))
  })
}


acc_rare_243_cf_lr
rare_243_cons$acc <- rbind(rare_243_cons$acc, c(method = "CF_LR", value = mean(acc_rare_243_cf_lr, na.rm = TRUE)))
rare_243_cons

#Time
time_per_batch_rare_243_cf_lr <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_cf_lr <- tryCatch({
    append(time_per_batch_rare_243_cf_lr, diff(result_rare_cf_lr_243[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_cf_lr, NA))
  })
}

time_per_batch_rare_243_cf_lr

rare_243_cons$time_per_batch <- rbind(rare_243_cons$time_per_batch,
                                      c(method = "CF_LR",
                                        value = mean(time_per_batch_rare_243_cf_lr, na.rm = TRUE)))
rare_243_cons



#ml_lstm
## ml lstm ===============================
ev_rare_243_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_ml_lstm[[j]] <- tryCatch({
    evaluate(result_rare_ml_lstm_243[[j]]$detector,
             result_rare_ml_lstm_243[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_243_ml_lstm

names(ev_rare_243_ml_lstm) <- names(rare_sample)[1:20]


ev_soft_rare_243_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_ml_lstm[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_ml_lstm_243[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_243_ml_lstm[[1]]
names(ev_soft_rare_243_ml_lstm) <- names(rare_sample)[1:20]

#F1
f1_rare_243_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_ml_lstm)) {
  f1_rare_243_ml_lstm <- tryCatch({
    append(f1_rare_243_ml_lstm, ev_rare_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_ml_lstm,NA))
  }) 
}


f1_rare_243_ml_lstm


rare_243_cons$f1 <- rbind(rare_243_cons$f1, c(method = "ML_LSTM", value = mean(f1_rare_243_ml_lstm, na.rm = TRUE)))

rare_243_cons


f1_soft_rare_243_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_ml_lstm)) {
  f1_soft_rare_243_ml_lstm <- tryCatch({
    append(f1_soft_rare_243_ml_lstm, ev_soft_rare_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_ml_lstm, NA))
  })
}

rare_243_cons$f1_soft <- rbind(rare_243_cons$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_rare_243_ml_lstm, na.rm = TRUE)))

rare_243_cons

#Accuracy
acc_rare_243_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_ml_lstm)) {
  acc_rare_243_ml_lstm <- tryCatch({
    append(acc_rare_243_ml_lstm, ev_rare_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_ml_lstm,NA))
  })
}


acc_rare_243_ml_lstm
rare_243_cons$acc <- rbind(rare_243_cons$acc, c(method = "ML_LSTM", value = mean(acc_rare_243_ml_lstm, na.rm = TRUE)))
rare_243_cons

#Time
time_per_batch_rare_243_ml_lstm <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_ml_lstm <- tryCatch({
    append(time_per_batch_rare_243_ml_lstm, diff(result_rare_ml_lstm_243[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_ml_lstm, NA))
  })
}

time_per_batch_rare_243_ml_lstm

rare_243_cons$time_per_batch <- rbind(rare_243_cons$time_per_batch,
                                      c(method = "ML_LSTM",
                                        value = mean(time_per_batch_rare_243_ml_lstm, na.rm = TRUE)))
rare_243_cons

##=====
## Save final results
save(rare_243_cons, file = "~/janio/harbinger/dev/results/rare_243_cons.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/rare_243_cons.RData")
rare_243_cons



## rare_81_mem9 -----------------------------------------------------------
#Run the next 4 lines only if decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_81_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_81_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_81_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_81_mem9.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_81_mem9.RData")


#Resume
rare_81_mem9_cons <- list()


## FBIAD -------------------------------------
ev_rare_81_mem9_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_mem9_fbiad[[j]] <- evaluate(result_rare_fbiad_81_mem9[[j]]$detector,
                                         result_rare_fbiad_81_mem9[[j]]$detection$event,
                                         rare_sample$event)
}


ev_rare_81_mem9_fbiad[[1]]
names(ev_rare_81_mem9_fbiad) <- names(rare_sample)[1:20]


ev_soft_rare_81_mem9_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_mem9_fbiad[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_fbiad_81_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_mem9_fbiad[[1]]
names(ev_soft_rare_81_mem9_fbiad) <- names(rare_sample)[1:20]


#F1
f1_rare_81_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_fbiad)) {
  f1_rare_81_mem9_fbiad <- tryCatch({
    append(f1_rare_81_mem9_fbiad, ev_rare_81_mem9_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_mem9_fbiad, NA))
  })
}


f1_rare_81_mem9_fbiad

rare_81_mem9_cons$f1 <- data.frame(method = "FBIAD", value = mean(f1_rare_81_mem9_fbiad, na.rm = TRUE))
rare_81_mem9_cons


f1_soft_rare_81_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_fbiad)) {
  f1_soft_rare_81_mem9_fbiad <- tryCatch({
    append(f1_soft_rare_81_mem9_fbiad, ev_soft_rare_81_mem9_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_mem9_fbiad, NA))
  })
}

rare_81_mem9_cons$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_rare_81_mem9_fbiad, na.rm = TRUE))

rare_81_mem9_cons

#Accuracy
acc_rare_81_mem9_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_fbiad)) {
  acc_rare_81_mem9_fbiad <- tryCatch({
    append(acc_rare_81_mem9_fbiad, ev_rare_81_mem9_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_mem9_fbiad, NA))
  })
}


acc_rare_81_mem9_fbiad
rare_81_mem9_cons$acc <- data.frame(method = "FBIAD", value = mean(acc_rare_81_mem9_fbiad, na.rm = TRUE))
rare_81_mem9_cons

#Time
result_rare_fbiad_81_mem9[[1]]$time
length(diff(result_rare_fbiad_81_mem9[[1]]$time))


time_per_batch_rare_81_mem9_fbiad <- c()
j = 1

for (j in 1:20) {
  time_per_batch_rare_81_mem9_fbiad <- tryCatch({
    append(
      time_per_batch_rare_81_mem9_fbiad,
      diff(result_rare_fbiad_81_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_mem9_fbiad, NA))
  })
}

time_per_batch_rare_81_mem9_fbiad
rare_81_mem9_cons$time_per_batch <- data.frame(method = "FBIAD",
                                               value = mean(time_per_batch_rare_81_mem9_fbiad, na.rm = TRUE))
rare_81_mem9_cons

#arima
##===============================
ev_rare_81_mem9_arima <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_mem9_arima[[j]] <- evaluate(result_rare_arima_81_mem9[[j]]$detector,
                                         result_rare_arima_81_mem9[[j]]$detection$event,
                                         rare_sample$event)
}


ev_rare_81_mem9_arima[[1]]
names(ev_rare_81_mem9_arima) <- names(rare_sample)[1:20]


ev_soft_rare_81_mem9_arima <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_mem9_arima[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_arima_81_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_mem9_arima[[1]]
names(ev_soft_rare_81_mem9_arima) <- names(rare_sample)[1:20]


#F1
f1_rare_81_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_arima)) {
  f1_rare_81_mem9_arima <- tryCatch({
    append(f1_rare_81_mem9_arima, ev_rare_81_mem9_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_mem9_arima, NA))
  })
}


f1_rare_81_mem9_arima


rare_81_mem9_cons$f1 <- rbind(rare_81_mem9_cons$f1, c(method = "ARIMA", value = mean(f1_rare_81_mem9_arima, na.rm = TRUE)))

rare_81_mem9_cons


f1_soft_rare_81_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_arima)) {
  f1_soft_rare_81_mem9_arima <- tryCatch({
    append(f1_soft_rare_81_mem9_arima, ev_soft_rare_81_mem9_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_mem9_arima, NA))
  })
}

rare_81_mem9_cons$f1_soft <- rbind(rare_81_mem9_cons$f1_soft, c(method = "ARIMA", value = mean(f1_soft_rare_81_mem9_arima, na.rm = TRUE)))

rare_81_mem9_cons

#Accuracy
acc_rare_81_mem9_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_arima)) {
  acc_rare_81_mem9_arima <- tryCatch({
    append(acc_rare_81_mem9_arima, ev_rare_81_mem9_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_mem9_arima, NA))
  })
}


acc_rare_81_mem9_arima
rare_81_mem9_cons$acc <- rbind(rare_81_mem9_cons$acc, c(method = "ARIMA", value = mean(acc_rare_81_mem9_arima, na.rm = TRUE)))
rare_81_mem9_cons

#Time
time_per_batch_rare_81_mem9_arima <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_mem9_arima <- tryCatch({
    append(
      time_per_batch_rare_81_mem9_arima,
      diff(result_rare_arima_81_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(
      time_per_batch_rare_81_mem9_arima, NA))
  })
}

time_per_batch_rare_81_mem9_arima

rare_81_mem9_cons$time_per_batch <- rbind(rare_81_mem9_cons$time_per_batch,
                                          c(method = "ARIMA",
                                            value = mean(time_per_batch_rare_81_mem9_arima, na.rm = TRUE)))
rare_81_mem9_cons


#garch ##===============================
ev_rare_81_mem9_garch <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_mem9_garch[[j]] <- tryCatch({
    evaluate(result_rare_garch_81_mem9[[j]]$detector,
             result_rare_garch_81_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_mem9_garch
names(ev_rare_81_mem9_garch) <- names(rare_sample)[1:20]


ev_soft_rare_81_mem9_garch <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_mem9_garch[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_garch_81_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_mem9_garch[[1]]
names(ev_soft_rare_81_mem9_garch) <- names(rare_sample)[1:20]


#F1
f1_rare_81_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_garch)) {
  f1_rare_81_mem9_garch <- tryCatch({
    append(f1_rare_81_mem9_garch, ev_rare_81_mem9_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_mem9_garch,NA))
  }) 
}


f1_rare_81_mem9_garch

rare_81_mem9_cons$f1 <- rbind(rare_81_mem9_cons$f1, c(method = "GARCH", value = mean(f1_rare_81_mem9_garch, na.rm = TRUE)))
rare_81_mem9_cons


f1_soft_rare_81_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_garch)) {
  f1_soft_rare_81_mem9_garch <- tryCatch({
    append(f1_soft_rare_81_mem9_garch, ev_soft_rare_81_mem9_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_mem9_garch, NA))
  })
}

rare_81_mem9_cons$f1_soft <- rbind(rare_81_mem9_cons$f1_soft, c(method = "GARCH", value = mean(f1_soft_rare_81_mem9_garch, na.rm = TRUE)))
rare_81_mem9_cons

#Accuracy
acc_rare_81_mem9_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_garch)) {
  acc_rare_81_mem9_garch <- tryCatch({
    append(acc_rare_81_mem9_garch, ev_rare_81_mem9_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_mem9_garch,NA))
  })
}


acc_rare_81_mem9_garch
rare_81_mem9_cons$acc <- rbind(rare_81_mem9_cons$acc, c(method = "GARCH", value = mean(acc_rare_81_mem9_garch, na.rm = TRUE)))
rare_81_mem9_cons

#Time
time_per_batch_rare_81_mem9_garch <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_mem9_garch <- tryCatch({
    append(time_per_batch_rare_81_mem9_garch, diff(result_rare_garch_81_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_mem9_garch, NA))
  })
}

time_per_batch_rare_81_mem9_garch

rare_81_mem9_cons$time_per_batch <- rbind(rare_81_mem9_cons$time_per_batch,
                                          c(method = "GARCH",
                                            value = mean(time_per_batch_rare_81_mem9_garch, na.rm = TRUE)))
rare_81_mem9_cons


#cf_lr ##===============================
ev_rare_81_mem9_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_mem9_cf_lr[[j]] <- tryCatch({
    evaluate(result_rare_cf_lr_81_mem9[[j]]$detector,
             result_rare_cf_lr_81_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_mem9_cf_lr

names(ev_rare_81_mem9_cf_lr) <- names(rare_sample)[1:20]


ev_soft_rare_81_mem9_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_mem9_cf_lr[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_cf_lr_81_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_mem9_cf_lr[[1]]
names(ev_soft_rare_81_mem9_cf_lr) <- names(rare_sample)[1:20]


#F1
f1_rare_81_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_cf_lr)) {
  f1_rare_81_mem9_cf_lr <- tryCatch({
    append(f1_rare_81_mem9_cf_lr, ev_rare_81_mem9_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_mem9_cf_lr,NA))
  }) 
}


f1_rare_81_mem9_cf_lr

rare_81_mem9_cons$f1 <- rbind(rare_81_mem9_cons$f1, c(method = "CF_LR", value = mean(f1_rare_81_mem9_cf_lr, na.rm = TRUE)))
rare_81_mem9_cons


f1_soft_rare_81_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_cf_lr)) {
  f1_soft_rare_81_mem9_cf_lr <- tryCatch({
    append(f1_soft_rare_81_mem9_cf_lr, ev_soft_rare_81_mem9_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_mem9_cf_lr, NA))
  })
}

rare_81_mem9_cons$f1_soft <- rbind(rare_81_mem9_cons$f1_soft, c(method = "CF_LR", value = mean(f1_soft_rare_81_mem9_cf_lr, na.rm = TRUE)))

rare_81_mem9_cons

#Accuracy
acc_rare_81_mem9_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_cf_lr)) {
  acc_rare_81_mem9_cf_lr <- tryCatch({
    append(acc_rare_81_mem9_cf_lr, ev_rare_81_mem9_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_mem9_cf_lr,NA))
  })
}


acc_rare_81_mem9_cf_lr
rare_81_mem9_cons$acc <- rbind(rare_81_mem9_cons$acc, c(method = "CF_LR", value = mean(acc_rare_81_mem9_cf_lr, na.rm = TRUE)))
rare_81_mem9_cons

#Time
time_per_batch_rare_81_mem9_cf_lr <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_mem9_cf_lr <- tryCatch({
    append(time_per_batch_rare_81_mem9_cf_lr, diff(result_rare_cf_lr_81_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_mem9_cf_lr, NA))
  })
}

time_per_batch_rare_81_mem9_cf_lr

rare_81_mem9_cons$time_per_batch <- rbind(rare_81_mem9_cons$time_per_batch,
                                          c(method = "CF_LR",
                                            value = mean(time_per_batch_rare_81_mem9_cf_lr, na.rm = TRUE)))
rare_81_mem9_cons


#ml_lstm - ##===============================
ev_rare_81_mem9_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_mem9_ml_lstm[[j]] <- tryCatch({
    evaluate(result_rare_ml_lstm_81_mem9[[j]]$detector,
             result_rare_ml_lstm_81_mem9[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_mem9_ml_lstm

names(ev_rare_81_mem9_ml_lstm) <- names(rare_sample)[1:20]


ev_soft_rare_81_mem9_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_mem9_ml_lstm[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_ml_lstm_81_mem9[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_mem9_ml_lstm[[1]]
names(ev_soft_rare_81_mem9_ml_lstm) <- names(rare_sample)[1:20]


#F1
f1_rare_81_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_ml_lstm)) {
  f1_rare_81_mem9_ml_lstm <- tryCatch({
    append(f1_rare_81_mem9_ml_lstm, ev_rare_81_mem9_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_mem9_ml_lstm,NA))
  }) 
}


f1_rare_81_mem9_ml_lstm

rare_81_mem9_cons$f1 <- rbind(rare_81_mem9_cons$f1, c(method = "ML_LSTM", value = mean(f1_rare_81_mem9_ml_lstm, na.rm = TRUE)))
rare_81_mem9_cons


f1_soft_rare_81_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_ml_lstm)) {
  f1_soft_rare_81_mem9_ml_lstm <- tryCatch({
    append(f1_soft_rare_81_mem9_ml_lstm, ev_soft_rare_81_mem9_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_mem9_ml_lstm, NA))
  })
}

rare_81_mem9_cons$f1_soft <- rbind(rare_81_mem9_cons$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_rare_81_mem9_ml_lstm, na.rm = TRUE)))

rare_81_mem9_cons


#Accuracy
acc_rare_81_mem9_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_mem9_ml_lstm)) {
  acc_rare_81_mem9_ml_lstm <- tryCatch({
    append(acc_rare_81_mem9_ml_lstm, ev_rare_81_mem9_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_mem9_ml_lstm,NA))
  })
}


acc_rare_81_mem9_ml_lstm
rare_81_mem9_cons$acc <- rbind(rare_81_mem9_cons$acc, c(method = "ML_LSTM", value = mean(acc_rare_81_mem9_ml_lstm, na.rm = TRUE)))
rare_81_mem9_cons

#Time
time_per_batch_rare_81_mem9_ml_lstm <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_mem9_ml_lstm <- tryCatch({
    append(time_per_batch_rare_81_mem9_ml_lstm, diff(result_rare_ml_lstm_81_mem9[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_mem9_ml_lstm, NA))
  })
}

time_per_batch_rare_81_mem9_ml_lstm

rare_81_mem9_cons$time_per_batch <- rbind(rare_81_mem9_cons$time_per_batch,
                                          c(method = "ML_LSTM",
                                            value = mean(time_per_batch_rare_81_mem9_ml_lstm, na.rm = TRUE)))
rare_81_mem9_cons

##=====
## Save final results
save(rare_81_mem9_cons, file = "~/janio/harbinger/dev/results/rare_81_mem9_cons.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/rare_81_mem9_cons.RData")
rare_81_mem9_cons


## rare_81 -----------------------------------------------------------
#Run the next 4 lines only if decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_81.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_81.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_81.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_81.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_81.RData")


#Resume
rare_81_cons <- list()


## FBIAD #----------------------------------
ev_rare_81_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_fbiad[[j]] <- evaluate(result_rare_fbiad_81[[j]]$detector,
                                    result_rare_fbiad_81[[j]]$detection$event,
                                    rare_sample$event)
}


ev_rare_81_fbiad[[1]]
names(ev_rare_81_fbiad) <- names(rare_sample)[1:20]


ev_soft_rare_81_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_fbiad[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_fbiad_81[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_fbiad[[1]]
names(ev_soft_rare_81_fbiad) <- names(rare_sample)[1:20]

#F1
f1_rare_81_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_fbiad)) {
  f1_rare_81_fbiad <- tryCatch({
    append(f1_rare_81_fbiad, ev_rare_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_fbiad, NA))
  })
}


f1_rare_81_fbiad

rare_81_cons$f1 <- data.frame(method = "FBIAD", value = mean(f1_rare_81_fbiad, na.rm = TRUE))
rare_81_cons


f1_soft_rare_81_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_fbiad)) {
  f1_soft_rare_81_fbiad <- tryCatch({
    append(f1_soft_rare_81_fbiad, ev_soft_rare_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_fbiad, NA))
  })
}

rare_81_cons$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_rare_81_fbiad, na.rm = TRUE))

rare_81_cons

#Accuracy
acc_rare_81_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_81_fbiad)) {
  acc_rare_81_fbiad <- tryCatch({
    append(acc_rare_81_fbiad, ev_rare_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_fbiad, NA))
  })
}


acc_rare_81_fbiad
rare_81_cons$acc <- data.frame(method = "FBIAD", value = mean(acc_rare_81_fbiad, na.rm = TRUE))
rare_81_cons

#Time
result_rare_fbiad_81[[1]]$time
length(diff(result_rare_fbiad_81[[1]]$time))


time_per_batch_rare_81_fbiad <- c()
j = 1

for (j in 1:20) {
  time_per_batch_rare_81_fbiad <- tryCatch({
    append(
      time_per_batch_rare_81_fbiad,
      diff(result_rare_fbiad_81[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_fbiad, NA))
  })
}

time_per_batch_rare_81_fbiad
rare_81_cons$time_per_batch <- data.frame(method = "FBIAD",
                                          value = mean(time_per_batch_rare_81_fbiad, na.rm = TRUE))
rare_81_cons

#arima ##===============================
ev_rare_81_arima <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_arima[[j]] <- evaluate(result_rare_arima_81[[j]]$detector,
                                    result_rare_arima_81[[j]]$detection$event,
                                    rare_sample$event)
}


ev_rare_81_arima[[1]]
names(ev_rare_81_arima) <- names(rare_sample)[1:20]


ev_soft_rare_81_arima <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_arima[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_arima_81[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_arima[[1]]
names(ev_soft_rare_81_arima) <- names(rare_sample)[1:20]

#F1
f1_rare_81_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_arima)) {
  f1_rare_81_arima <- tryCatch({
    append(f1_rare_81_arima, ev_rare_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_arima, NA))
  })
}


f1_rare_81_arima

rare_81_cons$f1 <- rbind(rare_81_cons$f1, c(method = "ARIMA", value = mean(f1_rare_81_arima, na.rm = TRUE)))
rare_81_cons



f1_soft_rare_81_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_arima)) {
  f1_soft_rare_81_arima <- tryCatch({
    append(f1_soft_rare_81_arima, ev_soft_rare_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_arima, NA))
  })
}

rare_81_cons$f1_soft <- rbind(rare_81_cons$f1_soft, c(method = "ARIMA", value = mean(f1_soft_rare_81_arima, na.rm = TRUE)))
rare_81_cons

#Accuracy
acc_rare_81_arima <- c()

j = 1

for (j in 1:length(ev_rare_81_arima)) {
  acc_rare_81_arima <- tryCatch({
    append(acc_rare_81_arima, ev_rare_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_arima, NA))
  })
}


acc_rare_81_arima
rare_81_cons$acc <- rbind(rare_81_cons$acc, c(method = "ARIMA", value = mean(acc_rare_81_arima, na.rm = TRUE)))
rare_81_cons

#Time
time_per_batch_rare_81_arima <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_arima <- tryCatch({
    append(
      time_per_batch_rare_81_arima,
      diff(result_rare_arima_81[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(
      time_per_batch_rare_81_arima, NA))
  })
}

time_per_batch_rare_81_arima

rare_81_cons$time_per_batch <- rbind(rare_81_cons$time_per_batch,
                                     c(method = "ARIMA",
                                       value = mean(time_per_batch_rare_81_arima, na.rm = TRUE)))
rare_81_cons


#garch ##===============================
ev_rare_81_garch <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_garch[[j]] <- tryCatch({
    evaluate(result_rare_garch_81[[j]]$detector,
             result_rare_garch_81[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_garch

names(ev_rare_81_garch) <- names(rare_sample)[1:20]


ev_soft_rare_81_garch <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_garch[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_garch_81[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_garch[[1]]
names(ev_soft_rare_81_garch) <- names(rare_sample)[1:20]


#F1
f1_rare_81_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_garch)) {
  f1_rare_81_garch <- tryCatch({
    append(f1_rare_81_garch, ev_rare_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_garch,NA))
  }) 
}


f1_rare_81_garch


rare_81_cons$f1 <- rbind(rare_81_cons$f1, c(method = "GARCH", value = mean(f1_rare_81_garch, na.rm = TRUE)))
rare_81_cons

f1_soft_rare_81_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_garch)) {
  f1_soft_rare_81_garch <- tryCatch({
    append(f1_soft_rare_81_garch, ev_soft_rare_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_garch, NA))
  })
}

rare_81_cons$f1_soft <- rbind(rare_81_cons$f1_soft, c(method = "GARCH", value = mean(f1_soft_rare_81_garch, na.rm = TRUE)))
rare_81_cons


#Accuracy
acc_rare_81_garch <- c()

j = 1

for (j in 1:length(ev_rare_81_garch)) {
  acc_rare_81_garch <- tryCatch({
    append(acc_rare_81_garch, ev_rare_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_garch,NA))
  })
}


acc_rare_81_garch
rare_81_cons$acc <- rbind(rare_81_cons$acc, c(method = "GARCH", value = mean(acc_rare_81_garch, na.rm = TRUE)))
rare_81_cons

#Time
time_per_batch_rare_81_garch <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_garch <- tryCatch({
    append(time_per_batch_rare_81_garch, diff(result_rare_garch_81[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_garch, NA))
  })
}

time_per_batch_rare_81_garch

rare_81_cons$time_per_batch <- rbind(rare_81_cons$time_per_batch,
                                     c(method = "GARCH",
                                       value = mean(time_per_batch_rare_81_garch, na.rm = TRUE)))
rare_81_cons


#cf_lr ##===============================
ev_rare_81_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_cf_lr[[j]] <- tryCatch({
    evaluate(result_rare_cf_lr_81[[j]]$detector,
             result_rare_cf_lr_81[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_cf_lr
names(ev_rare_81_cf_lr) <- names(rare_sample)[1:20]

ev_soft_rare_81_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_cf_lr[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_cf_lr_81[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_cf_lr[[1]]
names(ev_soft_rare_81_cf_lr) <- names(rare_sample)[1:20]


#F1
f1_rare_81_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_cf_lr)) {
  f1_rare_81_cf_lr <- tryCatch({
    append(f1_rare_81_cf_lr, ev_rare_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_cf_lr,NA))
  }) 
}


f1_rare_81_cf_lr

rare_81_cons$f1 <- rbind(rare_81_cons$f1, c(method = "CF_LR", value = mean(f1_rare_81_cf_lr, na.rm = TRUE)))
rare_81_cons


f1_soft_rare_81_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_cf_lr)) {
  f1_soft_rare_81_cf_lr <- tryCatch({
    append(f1_soft_rare_81_cf_lr, ev_soft_rare_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_cf_lr, NA))
  })
}

rare_81_cons$f1_soft <- rbind(rare_81_cons$f1_soft, c(method = "CF_LR", value = mean(f1_soft_rare_81_cf_lr, na.rm = TRUE)))

rare_81_cons

#Accuracy
acc_rare_81_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_81_cf_lr)) {
  acc_rare_81_cf_lr <- tryCatch({
    append(acc_rare_81_cf_lr, ev_rare_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_cf_lr,NA))
  })
}


acc_rare_81_cf_lr
rare_81_cons$acc <- rbind(rare_81_cons$acc, c(method = "CF_LR", value = mean(acc_rare_81_cf_lr, na.rm = TRUE)))
rare_81_cons

#Time
time_per_batch_rare_81_cf_lr <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_cf_lr <- tryCatch({
    append(time_per_batch_rare_81_cf_lr, diff(result_rare_cf_lr_81[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_cf_lr, NA))
  })
}

time_per_batch_rare_81_cf_lr

rare_81_cons$time_per_batch <- rbind(rare_81_cons$time_per_batch,
                                     c(method = "CF_LR",
                                       value = mean(time_per_batch_rare_81_cf_lr, na.rm = TRUE)))
rare_81_cons



#ml_lstm ##===============================
ev_rare_81_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_rare_81_ml_lstm[[j]] <- tryCatch({
    evaluate(result_rare_ml_lstm_81[[j]]$detector,
             result_rare_ml_lstm_81[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


ev_rare_81_ml_lstm

names(ev_rare_81_ml_lstm) <- names(rare_sample)[1:20]


ev_soft_rare_81_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_81_ml_lstm[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_ml_lstm_81[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

ev_soft_rare_81_ml_lstm[[1]]
names(ev_soft_rare_81_ml_lstm) <- names(rare_sample)[1:20]


#F1
f1_rare_81_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_ml_lstm)) {
  f1_rare_81_ml_lstm <- tryCatch({
    append(f1_rare_81_ml_lstm, ev_rare_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_81_ml_lstm,NA))
  }) 
}


f1_rare_81_ml_lstm

rare_81_cons$f1 <- rbind(rare_81_cons$f1, c(method = "ML_LSTM", value = mean(f1_rare_81_ml_lstm, na.rm = TRUE)))
rare_81_cons


f1_soft_rare_81_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_ml_lstm)) {
  f1_soft_rare_81_ml_lstm <- tryCatch({
    append(f1_soft_rare_81_ml_lstm, ev_soft_rare_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_81_ml_lstm, NA))
  })
}

rare_81_cons$f1_soft <- rbind(rare_81_cons$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_rare_81_ml_lstm, na.rm = TRUE)))

rare_81_cons

#Accuracy
acc_rare_81_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_81_ml_lstm)) {
  acc_rare_81_ml_lstm <- tryCatch({
    append(acc_rare_81_ml_lstm, ev_rare_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_81_ml_lstm,NA))
  })
}


acc_rare_81_ml_lstm
rare_81_cons$acc <- rbind(rare_81_cons$acc, c(method = "ML_LSTM", value = mean(acc_rare_81_ml_lstm, na.rm = TRUE)))
rare_81_cons

#Time
time_per_batch_rare_81_ml_lstm <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_81_ml_lstm <- tryCatch({
    append(time_per_batch_rare_81_ml_lstm, diff(result_rare_ml_lstm_81[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_81_ml_lstm, NA))
  })
}

time_per_batch_rare_81_ml_lstm

rare_81_cons$time_per_batch <- rbind(rare_81_cons$time_per_batch,
                                     c(method = "ML_LSTM",
                                       value = mean(time_per_batch_rare_81_ml_lstm, na.rm = TRUE)))
rare_81_cons

##=====
## Save final results
save(rare_81_cons, file = "~/janio/harbinger/dev/results/rare_81_cons.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/rare_81_cons.RData")
rare_81_cons



# Stream Result Analysis --------------------------------------------------
#Stream Result complete
str_rare_fbiad <- list()
str_rare_arima <- list()
str_rare_garch <- list()
str_rare_cf <- list()
str_rare_lstm <- list()

#Lag analysis
lag_rare_fbiad <- c()
lag_rare_arima <- c()
lag_rare_garch <- c()
lag_rare_cf <- c()
lag_rare_lstm <- c()

lag_rare_overall <- list()

## STR Evaluate

##===========================================================================
##===========================================================================
#FBIAD ====
#Group evaluate
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_rare_fbiad_243)){
  str_rare_fbiad[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_rare_fbiad_243[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_rare_fbiad[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_rare_fbiad) <- names(result_rare_fbiad_243)

#General evaluate
lag_rare_fbiad <- tryCatch({
  append(lag_rare_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_rare_fbiad, NA)
})


#ARIMA ====
#Group evaluate
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_rare_arima_243)){
  str_rare_arima[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_rare_arima_243[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_rare_arima[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_rare_arima) <- names(result_rare_arima_243)

#General evaluate
lag_rare_arima <- tryCatch({
  append(lag_rare_arima, lag_partial)
}, error = function(e) {
  append(lag_rare_arima, NA)
})


#GARCH ====
#Group evaluate
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_rare_garch_243)){
  str_rare_garch[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_rare_garch_243[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_rare_garch[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_rare_garch) <- names(result_rare_garch_243)

#General evaluate
lag_rare_garch <- tryCatch({
  append(lag_rare_garch, lag_partial)
}, error = function(e) {
  append(lag_rare_garch, NA)
})


#CF ====
#Group evaluate
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_rare_cf_lr_243)){
  str_rare_cf[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_rare_cf_lr_243[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_rare_cf[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_rare_cf) <- names(result_rare_cf_lr_243)

#General evaluate
lag_rare_cf <- tryCatch({
  append(lag_rare_cf, lag_partial)
}, error = function(e) {
  append(lag_rare_cf, NA)
})


#LSTM ====
#Group evaluate
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_rare_ml_lstm_243)){
  str_rare_lstm[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_rare_ml_lstm_243[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_rare_lstm[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_rare_lstm) <- names(result_rare_ml_lstm_243)

#General evaluate
lag_rare_lstm <- tryCatch({
  append(lag_rare_lstm, lag_partial)
}, error = function(e) {
  append(lag_rare_lstm, NA)
})

lag_rare_overall


##===========================================================================
##===========================================================================


## STR Consolidation ====
lag_rare_overall$complete <-data.frame(method = "FBIAD",
                                       mean = mean(lag_rare_fbiad, na.rm = TRUE),
                                       median = median(lag_rare_fbiad, na.rm = TRUE),
                                       max = max(lag_rare_fbiad, na.rm = TRUE),
                                       min = min(lag_rare_fbiad, na.rm = TRUE))

lag_rare_overall$complete <- rbind(lag_rare_overall$complete, c(method = "ARIMA",
                                                                mean = mean(lag_rare_arima, na.rm = TRUE),
                                                                median = median(lag_rare_arima, na.rm = TRUE),
                                                                max = max(lag_rare_arima, na.rm = TRUE),
                                                                min = min(lag_rare_arima, na.rm = TRUE)))

lag_rare_overall$complete <- rbind(lag_rare_overall$complete, c(method = "GARCH",
                                                                mean = mean(lag_rare_garch, na.rm = TRUE),
                                                                median = median(lag_rare_garch, na.rm = TRUE),
                                                                max = max(lag_rare_garch, na.rm = TRUE),
                                                                min = min(lag_rare_garch, na.rm = TRUE)))

lag_rare_overall$complete <- rbind(lag_rare_overall$complete, c(method = "CF",
                                                                mean = mean(lag_rare_cf, na.rm = TRUE),
                                                                median = median(lag_rare_cf, na.rm = TRUE),
                                                                max = max(lag_rare_cf, na.rm = TRUE),
                                                                min = min(lag_rare_cf, na.rm = TRUE)))

lag_rare_overall$complete <- rbind(lag_rare_overall$complete, c(method = "LSTM",
                                                                mean = mean(lag_rare_lstm, na.rm = TRUE),
                                                                median = median(lag_rare_lstm, na.rm = TRUE),
                                                                max = max(lag_rare_lstm, na.rm = TRUE),
                                                                min = min(lag_rare_lstm, na.rm = TRUE)))



lag_rare_overall
save(lag_rare_overall, file = "~/janio/harbinger/dev/results/lag_rare_overall.RData", compress = TRUE)




# Complementary experiments - RARE 243 27 ---------------------------------
## rare_243_27 -----------------------------------------------------------
#Run the next 4 lines only if you decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_243_27.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_243_27.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_243_27.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_243_27.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_243_27.RData")


#Resume
rare_243_27_cons <- list()


## FBIAD #----------------------------------
ev_rare_243_27_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_27_fbiad[[j]] <- evaluate(result_rare_fbiad_243_27[[j]]$detector,
                                        result_rare_fbiad_243_27[[j]]$detection$event,
                                        rare_sample$event)
}


names(ev_rare_243_27_fbiad) <- names(rare_sample)[1:20]


ev_soft_rare_243_27_fbiad <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_27_fbiad[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_fbiad_243_27[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_rare_243_27_fbiad) <- names(rare_sample)[1:20]

##METRICS
#Accuracy
acc_rare_243_27_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_27_fbiad)) {
  acc_rare_243_27_fbiad <- tryCatch({
    append(acc_rare_243_27_fbiad, ev_rare_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_27_fbiad, NA))
  })
}

rare_243_27_cons$acc <- data.frame(method = "FBIAD", value = mean(acc_rare_243_27_fbiad, na.rm = TRUE))
rare_243_27_cons$acc


#F1
f1_rare_243_27_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_27_fbiad)) {
  f1_rare_243_27_fbiad <- tryCatch({
    append(f1_rare_243_27_fbiad, ev_rare_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_27_fbiad, NA))
  })
}


rare_243_27_cons$f1 <- data.frame(method = "FBIAD", value = mean(f1_rare_243_27_fbiad, na.rm = TRUE))
rare_243_27_cons$f1


#F1 SOFT
f1_soft_rare_243_27_fbiad <- c()

j = 1

for (j in 1:length(ev_rare_243_27_fbiad)) {
  f1_soft_rare_243_27_fbiad <- tryCatch({
    append(f1_soft_rare_243_27_fbiad, ev_soft_rare_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_27_fbiad, NA))
  })
}

rare_243_27_cons$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_rare_243_27_fbiad, na.rm = TRUE))
rare_243_27_cons$f1_soft


#Time
result_rare_fbiad_243_27[[1]]$time
length(diff(result_rare_fbiad_243_27[[1]]$time))


time_per_batch_rare_243_27_fbiad <- c()
j = 1

for (j in 1:20) {
  time_per_batch_rare_243_27_fbiad <- tryCatch({
    append(
      time_per_batch_rare_243_27_fbiad,
      diff(result_rare_fbiad_243_27[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_27_fbiad, NA))
  })
}

rare_243_27_cons$time_per_batch <- data.frame(method = "FBIAD",
                                              value = mean(time_per_batch_rare_243_27_fbiad, na.rm = TRUE))
rare_243_27_cons

#arima ##===============================
ev_rare_243_27_arima <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_27_arima[[j]] <- evaluate(result_rare_arima_243_27[[j]]$detector,
                                        result_rare_arima_243_27[[j]]$detection$event,
                                        rare_sample$event)
}


names(ev_rare_243_27_arima) <- names(rare_sample)[1:20]


ev_soft_rare_243_27_arima <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_27_arima[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_arima_243_27[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_rare_243_27_arima) <- names(rare_sample)[1:20]


#Accuracy
acc_rare_243_27_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_27_arima)) {
  acc_rare_243_27_arima <- tryCatch({
    append(acc_rare_243_27_arima, ev_rare_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_27_arima, NA))
  })
}


rare_243_27_cons$acc <- rbind(rare_243_27_cons$acc, c(method = "ARIMA", value = mean(acc_rare_243_27_arima, na.rm = TRUE)))
rare_243_27_cons$acc

#F1
f1_rare_243_27_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_27_arima)) {
  f1_rare_243_27_arima <- tryCatch({
    append(f1_rare_243_27_arima, ev_rare_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_27_arima, NA))
  })
}


rare_243_27_cons$f1 <- rbind(rare_243_27_cons$f1, c(method = "ARIMA", value = mean(f1_rare_243_27_arima, na.rm = TRUE)))
rare_243_27_cons$f1



f1_soft_rare_243_27_arima <- c()

j = 1

for (j in 1:length(ev_rare_243_27_arima)) {
  f1_soft_rare_243_27_arima <- tryCatch({
    append(f1_soft_rare_243_27_arima, ev_soft_rare_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_27_arima, NA))
  })
}

rare_243_27_cons$f1_soft <- rbind(rare_243_27_cons$f1_soft, c(method = "ARIMA", value = mean(f1_soft_rare_243_27_arima, na.rm = TRUE)))
rare_243_27_cons$f1_soft



#Time
time_per_batch_rare_243_27_arima <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_27_arima <- tryCatch({
    append(
      time_per_batch_rare_243_27_arima,
      diff(result_rare_arima_243_27[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(
      time_per_batch_rare_243_27_arima, NA))
  })
}

time_per_batch_rare_243_27_arima

rare_243_27_cons$time_per_batch <- rbind(rare_243_27_cons$time_per_batch,
                                         c(method = "ARIMA",
                                           value = mean(time_per_batch_rare_243_27_arima, na.rm = TRUE)))
rare_243_27_cons


#garch ##===============================
ev_rare_243_27_garch <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_27_garch[[j]] <- tryCatch({
    evaluate(result_rare_garch_243_27[[j]]$detector,
             result_rare_garch_243_27[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


names(ev_rare_243_27_garch) <- names(rare_sample)[1:20]


ev_soft_rare_243_27_garch <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_27_garch[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_garch_243_27[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_rare_243_27_garch) <- names(rare_sample)[1:20]

##METRCIS
#Accuracy
acc_rare_243_27_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_27_garch)) {
  acc_rare_243_27_garch <- tryCatch({
    append(acc_rare_243_27_garch, ev_rare_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_27_garch,NA))
  })
}


rare_243_27_cons$acc <- rbind(rare_243_27_cons$acc, c(method = "GARCH", value = mean(acc_rare_243_27_garch, na.rm = TRUE)))
rare_243_27_cons$acc


#F1
f1_rare_243_27_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_27_garch)) {
  f1_rare_243_27_garch <- tryCatch({
    append(f1_rare_243_27_garch, ev_rare_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_27_garch,NA))
  }) 
}


rare_243_27_cons$f1 <- rbind(rare_243_27_cons$f1, c(method = "GARCH", value = mean(f1_rare_243_27_garch, na.rm = TRUE)))
rare_243_27_cons$f1


#F1 SOFT
f1_soft_rare_243_27_garch <- c()

j = 1

for (j in 1:length(ev_rare_243_27_garch)) {
  f1_soft_rare_243_27_garch <- tryCatch({
    append(f1_soft_rare_243_27_garch, ev_soft_rare_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_27_garch, NA))
  })
}

rare_243_27_cons$f1_soft <- rbind(rare_243_27_cons$f1_soft, c(method = "GARCH", value = mean(f1_soft_rare_243_27_garch, na.rm = TRUE)))
rare_243_27_cons$f1_soft


#Time
time_per_batch_rare_243_27_garch <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_27_garch <- tryCatch({
    append(time_per_batch_rare_243_27_garch, diff(result_rare_garch_243_27[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_27_garch, NA))
  })
}


rare_243_27_cons$time_per_batch <- rbind(rare_243_27_cons$time_per_batch,
                                         c(method = "GARCH",
                                           value = mean(time_per_batch_rare_243_27_garch, na.rm = TRUE)))
rare_243_27_cons


#cf_lr ##===============================
ev_rare_243_27_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_27_cf_lr[[j]] <- tryCatch({
    evaluate(result_rare_cf_lr_243_27[[j]]$detector,
             result_rare_cf_lr_243_27[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}


names(ev_rare_243_27_cf_lr) <- names(rare_sample)[1:20]

ev_soft_rare_243_27_cf_lr <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_27_cf_lr[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_cf_lr_243_27[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_rare_243_27_cf_lr) <- names(rare_sample)[1:20]

##METRICS
#Accuracy
acc_rare_243_27_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_27_cf_lr)) {
  acc_rare_243_27_cf_lr <- tryCatch({
    append(acc_rare_243_27_cf_lr, ev_rare_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_27_cf_lr,NA))
  })
}

rare_243_27_cons$acc <- rbind(rare_243_27_cons$acc, c(method = "CF_LR", value = mean(acc_rare_243_27_cf_lr, na.rm = TRUE)))
rare_243_27_cons$acc

#F1
f1_rare_243_27_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_27_cf_lr)) {
  f1_rare_243_27_cf_lr <- tryCatch({
    append(f1_rare_243_27_cf_lr, ev_rare_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_27_cf_lr,NA))
  }) 
}

rare_243_27_cons$f1 <- rbind(rare_243_27_cons$f1, c(method = "CF_LR", value = mean(f1_rare_243_27_cf_lr, na.rm = TRUE)))
rare_243_27_cons$f1

#F1 SOFT
f1_soft_rare_243_27_cf_lr <- c()

j = 1

for (j in 1:length(ev_rare_243_27_cf_lr)) {
  f1_soft_rare_243_27_cf_lr <- tryCatch({
    append(f1_soft_rare_243_27_cf_lr, ev_soft_rare_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_27_cf_lr, NA))
  })
}

rare_243_27_cons$f1_soft <- rbind(rare_243_27_cons$f1_soft, c(method = "CF_LR", value = mean(f1_soft_rare_243_27_cf_lr, na.rm = TRUE)))

rare_243_27_cons$f1_soft


#Time
time_per_batch_rare_243_27_cf_lr <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_27_cf_lr <- tryCatch({
    append(time_per_batch_rare_243_27_cf_lr, diff(result_rare_cf_lr_243_27[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_27_cf_lr, NA))
  })
}

rare_243_27_cons$time_per_batch <- rbind(rare_243_27_cons$time_per_batch,
                                         c(method = "CF_LR",
                                           value = mean(time_per_batch_rare_243_27_cf_lr, na.rm = TRUE)))
rare_243_27_cons


#ml_lstm ##===============================
ev_rare_243_27_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_rare_243_27_ml_lstm[[j]] <- tryCatch({
    evaluate(result_rare_ml_lstm_243_27[[j]]$detector,
             result_rare_ml_lstm_243_27[[j]]$detection$event,
             rare_sample$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_rare_243_27_ml_lstm) <- names(rare_sample)[1:20]


ev_soft_rare_243_27_ml_lstm <- list()

j = 1

for (j in 1:20) {
  ev_soft_rare_243_27_ml_lstm[[j]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_rare_ml_lstm_243_27[[j]]$detection$event),
             as.logical(rare_sample$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_rare_243_27_ml_lstm) <- names(rare_sample)[1:20]

##METRICS

#Accuracy
acc_rare_243_27_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_27_ml_lstm)) {
  acc_rare_243_27_ml_lstm <- tryCatch({
    append(acc_rare_243_27_ml_lstm, ev_rare_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_27_ml_lstm,NA))
  })
}

rare_243_27_cons$acc <- rbind(rare_243_27_cons$acc, c(method = "ML_LSTM", value = mean(acc_rare_243_27_ml_lstm, na.rm = TRUE)))
rare_243_27_cons$acc

#F1
f1_rare_243_27_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_27_ml_lstm)) {
  f1_rare_243_27_ml_lstm <- tryCatch({
    append(f1_rare_243_27_ml_lstm, ev_rare_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_27_ml_lstm,NA))
  }) 
}

rare_243_27_cons$f1 <- rbind(rare_243_27_cons$f1, c(method = "ML_LSTM", value = mean(f1_rare_243_27_ml_lstm, na.rm = TRUE)))
rare_243_27_cons$f1

#F1 SOFT
f1_soft_rare_243_27_ml_lstm <- c()

j = 1

for (j in 1:length(ev_rare_243_27_ml_lstm)) {
  f1_soft_rare_243_27_ml_lstm <- tryCatch({
    append(f1_soft_rare_243_27_ml_lstm, ev_soft_rare_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_rare_243_27_ml_lstm, NA))
  })
}

rare_243_27_cons$f1_soft <- rbind(rare_243_27_cons$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_rare_243_27_ml_lstm, na.rm = TRUE)))
rare_243_27_cons$f1_soft


#Time
time_per_batch_rare_243_27_ml_lstm <- c()

j = 1

for (j in 1:20) {
  time_per_batch_rare_243_27_ml_lstm <- tryCatch({
    append(time_per_batch_rare_243_27_ml_lstm, diff(result_rare_ml_lstm_243_27[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_per_batch_rare_243_27_ml_lstm, NA))
  })
}


rare_243_27_cons$time_per_batch <- rbind(rare_243_27_cons$time_per_batch,
                                         c(method = "ML_LSTM",
                                           value = mean(time_per_batch_rare_243_27_ml_lstm, na.rm = TRUE)))
rare_243_27_cons

##=====
## Save final results
save(rare_243_27_cons, file = "~/janio/harbinger/dev/results/rare_243_27_cons.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/rare_243_27_cons.RData")
rare_243_27_cons