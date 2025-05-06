#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")

## art_anomaly -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_art_anom.RData")
data(numenta_artificialWithAnomaly)

#Resume
numenta_81_cons <- list()
numenta_81_cons_overall <- list()


#FBIAD =========
ev_numenta_81_fbiad <- list()

k = 1
for (k in 1:length(result_81_fbiad_numenta_art_anom)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_art_anom[[k]]$detector,
             result_81_fbiad_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_81_fbiad <- list()

k = 1
for (k in 1:length(result_81_fbiad_numenta_art_anom)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_81_fbiad <- c() #All series to create overall results
numenta_81_cons$acc <- data.frame() #By Series


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_numenta_81_fbiad <- c() #All series to create overall results
numenta_81_cons$f1 <- data.frame() #By Series


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                             c(method = "FBIAD_ArtAnom",
                               value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_numenta_81_fbiad <- c() #All series to create overall results
numenta_81_cons$f1_soft <- data.frame() #By Series


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
result_81_fbiad_numenta_art_anom[[1]]$time
diff(result_81_fbiad_numenta_art_anom[[1]]$time)
length(diff(result_81_fbiad_numenta_art_anom[[1]]$time))

time_per_batch_numenta_81_fbiad <- c() #All series
numenta_81_cons$time_per_batch <- data.frame()
numenta_81_cons_overall$time_per_batch <- data.frame()


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_art_anom)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_art_anom[[k]]$detector,
             result_81_arima_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_art_anom)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_81_arima <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_numenta_81_arima <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_ArtAnom",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_numenta_81_arima <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_per_batch_numenta_81_arima <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_art_anom)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_art_anom[[k]]$detector,
             result_81_garch_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_art_anom)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_81_garch <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_numenta_81_garch <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_ArtAnom",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_numenta_81_garch <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_per_batch_numenta_81_garch <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_art_anom)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_art_anom[[k]]$detector,
             result_81_cf_lr_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_art_anom)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_81_cf_lr <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_numenta_81_cf_lr <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_ArtAnom",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_numenta_81_cf_lr <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_per_batch_numenta_81_cf_lr <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_art_anom)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_art_anom[[k]]$detector,
             result_81_ml_lstm_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_art_anom)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_81_ml_lstm <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_numenta_81_ml_lstm <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_ArtAnom",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_numenta_81_ml_lstm <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_per_batch_numenta_81_ml_lstm <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## REAL AD EXC -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_real_adexc.RData")
data(numenta_realAdExchange)


#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_art_anom)
rm(result_81_cf_lr_numenta_art_anom)
rm(result_81_fbiad_numenta_art_anom)
rm(result_81_garch_numenta_art_anom)
rm(result_81_ml_lstm_numenta_art_anom)
rm(numenta_artificialWithAnomaly)



#FBIAD =========
ev_numenta_81_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_adexc)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_real_adexc[[k]]$detector,
             result_81_fbiad_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_81_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_adexc)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_ReadAdExch",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "FBIAD_ReadAdExch",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_ReadAdExch",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_ReadAdExch",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_adexc)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_real_adexc[[k]]$detector,
             result_81_arima_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_adexc)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_ReadAdExch",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_ReadAdExch",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_ReadAdExch",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
#ReadAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_ReadAdExch",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_adexc)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_real_adexc[[k]]$detector,
             result_81_garch_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_adexc)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_ReadAdExch",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_ReadAdExch",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_ReadAdExch",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_ReadAdExch",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_adexc)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_real_adexc[[k]]$detector,
             result_81_cf_lr_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_adexc)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_ReadAdExch",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_ReadAdExch",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_ReadAdExch",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#ReadAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_ReadAdExch",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_adexc)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_real_adexc[[k]]$detector,
             result_81_ml_lstm_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_adexc)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_ReadAdExch",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_ReadAdExch",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_ReadAdExch",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#ReadAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_ReadAdExch",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_real_adexc)
rm(result_81_cf_lr_numenta_real_adexc)
rm(result_81_fbiad_numenta_real_adexc)
rm(result_81_garch_numenta_real_adexc)
rm(result_81_ml_lstm_numenta_real_adexc)
rm(numenta_realAdExchange)


## REAL AWS -----------------------------------------------------------
data(numenta_realAWSCloudwatch)

#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_real_aws.RData")



#FBIAD =========
ev_numenta_81_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_aws)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_real_aws[[k]]$detector,
             result_81_fbiad_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_81_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_aws)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_RealAWS",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "FBIAD_RealAWS",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_RealAWS",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_RealAWS",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_aws)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_real_aws[[k]]$detector,
             result_81_arima_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_aws)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_RealAWS",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_RealAWS",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_RealAWS",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_RealAWS",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_aws)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_real_aws[[k]]$detector,
             result_81_garch_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_aws)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_RealAWS",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_RealAWS",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_RealAWS",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_RealAWS",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_aws)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_real_aws[[k]]$detector,
             result_81_cf_lr_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_aws)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_RealAWS",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_RealAWS",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_RealAWS",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_RealAWS",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_aws)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_real_aws[[k]]$detector,
             result_81_ml_lstm_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_aws)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_RealAWS",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_RealAWS",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_RealAWS",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_RealAWS",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_real_aws)
rm(result_81_cf_lr_numenta_real_aws)
rm(result_81_fbiad_numenta_real_aws)
rm(result_81_garch_numenta_real_aws)
rm(result_81_ml_lstm_numenta_real_aws)
rm(numenta_realAWSCloudwatch)



## REAL Known Cause ----------------------------------------------------
data(numenta_realKnownCause)

#Before use loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_real_knwcs.RData")



#FBIAD =========
ev_numenta_81_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_knwcs)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_real_knwcs[[k]]$detector,
             result_81_fbiad_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_81_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_knwcs)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_RealKnownCause",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "FBIAD_RealKnownCause",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_RealKnownCause",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_RealKnownCause",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_knwcs)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_real_knwcs[[k]]$detector,
             result_81_arima_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_knwcs)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_RealKnownCause",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_RealKnownCause",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_RealKnownCause",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_RealKnownCause",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_knwcs)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_real_knwcs[[k]]$detector,
             result_81_garch_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_knwcs)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_RealKnownCause",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_RealKnownCause",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_RealKnownCause",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_RealKnownCause",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_knwcs)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_real_knwcs[[k]]$detector,
             result_81_cf_lr_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_knwcs)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_RealKnownCause",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_RealKnownCause",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_RealKnownCause",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_RealKnownCause",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_knwcs)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_real_knwcs[[k]]$detector,
             result_81_ml_lstm_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_knwcs)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_RealKnownCause",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_RealKnownCause",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_RealKnownCause",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_RealKnownCause",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_real_knwcs)
rm(result_81_cf_lr_numenta_real_knwcs)
rm(result_81_fbiad_numenta_real_knwcs)
rm(result_81_garch_numenta_real_knwcs)
rm(result_81_ml_lstm_numenta_real_knwcs)
rm(numenta_RealKnownCause)



## REAL Real Traffic ----------------------------------------------------
data(numenta_realTraffic)

#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_real_traff.RData")


#FBIAD =========
ev_numenta_81_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_traff)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_real_traff[[k]]$detector,
             result_81_fbiad_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_81_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_traff)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_RealTraffic",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "FBIAD_RealTraffic",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_RealTraffic",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_RealTraffic",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_traff)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_real_traff[[k]]$detector,
             result_81_arima_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_traff)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_RealTraffic",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_RealTraffic",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_RealTraffic",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_RealTraffic",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_traff)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_real_traff[[k]]$detector,
             result_81_garch_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_traff)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_RealTraffic",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_RealTraffic",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_RealTraffic",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_RealTraffic",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_traff)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_real_traff[[k]]$detector,
             result_81_cf_lr_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_traff)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_RealTraffic",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_RealTraffic",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_RealTraffic",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_RealTraffic",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_traff)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_real_traff[[k]]$detector,
             result_81_ml_lstm_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_traff)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_RealTraffic",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_RealTraffic",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_RealTraffic",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_RealTraffic",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_real_traff)
rm(result_81_cf_lr_numenta_real_traff)
rm(result_81_fbiad_numenta_real_traff)
rm(result_81_garch_numenta_real_traff)
rm(result_81_ml_lstm_numenta_real_traff)
rm(numenta_realTraffic)


###=====================================
###=====================================
#Code copied - Adjust before running
## REAL Real Tweets ----------------------------------------------------
data(numenta_realTweets)

#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_81_fbiad_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_81_arima_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_81_garch_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_81_cf_lr_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_81_ml_lstm_numenta_real_tweets.RData")


#FBIAD =========
ev_numenta_81_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_tweets)) {
  ev_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(result_81_fbiad_numenta_real_tweets[[k]]$detector,
             result_81_fbiad_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_fbiad) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_81_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_81_fbiad_numenta_real_tweets)) {
  ev_soft_numenta_81_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_fbiad) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_fbiad <- append(acc_numenta_81_fbiad, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "FBIAD_RealTweets",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_fbiad <- append(f1_numenta_81_fbiad, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "FBIAD_RealTweets",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_fbiad <- append(f1_soft_numenta_81_fbiad, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "FBIAD_RealTweets",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_fbiad <- append(time_per_batch_numenta_81_fbiad,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "FBIAD_RealTweets",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ARIMA =========
ev_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_tweets)) {
  ev_numenta_81_arima[[k]] <- tryCatch({
    evaluate(result_81_arima_numenta_real_tweets[[k]]$detector,
             result_81_arima_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_arima) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_81_arima <- list()

k = 1
for (k in 1:length(result_81_arima_numenta_real_tweets)) {
  ev_soft_numenta_81_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_arima) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_arima <- append(acc_numenta_81_arima, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ARIMA_RealTweets",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_arima <- append(f1_numenta_81_arima, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ARIMA_RealTweets",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_arima <- append(f1_soft_numenta_81_arima, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ARIMA_RealTweets",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_arima <- append(time_per_batch_numenta_81_arima,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ARIMA_RealTweets",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#GARCH =========
ev_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_tweets)) {
  ev_numenta_81_garch[[k]] <- tryCatch({
    evaluate(result_81_garch_numenta_real_tweets[[k]]$detector,
             result_81_garch_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_garch) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_81_garch <- list()

k = 1
for (k in 1:length(result_81_garch_numenta_real_tweets)) {
  ev_soft_numenta_81_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_garch) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_garch <- append(acc_numenta_81_garch, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "GARCH_RealTweets",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_garch <- append(f1_numenta_81_garch, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "GARCH_RealTweets",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_garch <- append(f1_soft_numenta_81_garch, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "GARCH_RealTweets",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_garch <- append(time_per_batch_numenta_81_garch,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "GARCH_RealTweets",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#CF_LR =========
ev_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_tweets)) {
  ev_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_numenta_real_tweets[[k]]$detector,
             result_81_cf_lr_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_cf_lr) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_81_cf_lr <- list()

k = 1
for (k in 1:length(result_81_cf_lr_numenta_real_tweets)) {
  ev_soft_numenta_81_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_cf_lr) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_cf_lr <- append(acc_numenta_81_cf_lr, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "CF_LR_RealTweets",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_cf_lr <- append(f1_numenta_81_cf_lr, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "CF_LR_RealTweets",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_cf_lr <- append(f1_soft_numenta_81_cf_lr, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "CF_LR_RealTweets",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_cf_lr_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_cf_lr_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_cf_lr <- append(time_per_batch_numenta_81_cf_lr,
                                          time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "CF_LR_RealTweets",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


#ML_LSTM =========
ev_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_tweets)) {
  ev_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_numenta_real_tweets[[k]]$detector,
             result_81_ml_lstm_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_81_ml_lstm) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_81_ml_lstm <- list()

k = 1
for (k in 1:length(result_81_ml_lstm_numenta_real_tweets)) {
  ev_soft_numenta_81_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_81_ml_lstm) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_81_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_81_ml_lstm <- append(acc_numenta_81_ml_lstm, acc_partial)
}

numenta_81_cons$acc <- rbind(numenta_81_cons$acc,
                             c(method = "ML_LSTM_RealTweets",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_81_cons$acc) <- c("method", "value")
numenta_81_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_81_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_81_ml_lstm <- append(f1_numenta_81_ml_lstm, f1_partial)
}

numenta_81_cons$f1 <- rbind(numenta_81_cons$f1,
                            c(method = "ML_LSTM_RealTweets",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_81_cons$f1) <- c("method", "value")
numenta_81_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_81_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_81_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_81_ml_lstm <- append(f1_soft_numenta_81_ml_lstm, f1_soft_partial)
}

numenta_81_cons$f1_soft <- rbind(numenta_81_cons$f1_soft,
                                 c(method = "ML_LSTM_RealTweets",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_81_cons$f1_soft) <- c("method", "value")
numenta_81_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_81_ml_lstm <- append(time_per_batch_numenta_81_ml_lstm,
                                            time_bt_partial)


numenta_81_cons$time_per_batch <- rbind(numenta_81_cons$time_per_batch,
                                        c(method = "ML_LSTM_RealTweets",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_81_cons$time_per_batch) <- c("method", "value")
numenta_81_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_81_arima_numenta_real_tweets)
rm(result_81_cf_lr_numenta_real_tweets)
rm(result_81_fbiad_numenta_real_tweets)
rm(result_81_garch_numenta_real_tweets)
rm(result_81_ml_lstm_numenta_real_tweets)
rm(numenta_realTweets)



###=====================================
###=====================================


##=====
## Save final results
save(numenta_81_cons, file = "~/janio/harbinger/dev/results/numenta_81_cons.RData", compress = TRUE)
numenta_81_cons

##=====
##Calculate overall metrics (RUN AFTER COMPLETING ALL GROUPS)
numenta_81_cons_overall <- list()

##ACC
numenta_81_cons_overall$acc <- data.frame(method = "FBIAD", value = mean(acc_numenta_81_fbiad, na.rm = TRUE))
numenta_81_cons_overall$acc <- rbind(numenta_81_cons_overall$acc, c(method = "ARIMA", value = mean(acc_numenta_81_arima, na.rm = TRUE)))
numenta_81_cons_overall$acc <- rbind(numenta_81_cons_overall$acc, c(method = "GARCH", value = mean(acc_numenta_81_garch, na.rm = TRUE)))
numenta_81_cons_overall$acc <- rbind(numenta_81_cons_overall$acc, c(method = "CF_LR", value = mean(acc_numenta_81_cf_lr, na.rm = TRUE)))
numenta_81_cons_overall$acc <- rbind(numenta_81_cons_overall$acc, c(method = "ML_LSTM", value = mean(acc_numenta_81_ml_lstm, na.rm = TRUE)))

##F1
numenta_81_cons_overall$f1 <- data.frame(method = "FBIAD", value = mean(f1_numenta_81_fbiad, na.rm = TRUE))
numenta_81_cons_overall$f1 <- rbind(numenta_81_cons_overall$f1, c(method = "ARIMA", value = mean(f1_numenta_81_arima, na.rm = TRUE)))
numenta_81_cons_overall$f1 <- rbind(numenta_81_cons_overall$f1, c(method = "GARCH", value = mean(f1_numenta_81_garch, na.rm = TRUE)))
numenta_81_cons_overall$f1 <- rbind(numenta_81_cons_overall$f1, c(method = "CF_LR", value = mean(f1_numenta_81_cf_lr, na.rm = TRUE)))
numenta_81_cons_overall$f1 <- rbind(numenta_81_cons_overall$f1, c(method = "ML_LSTM", value = mean(f1_numenta_81_ml_lstm, na.rm = TRUE)))

##F1 SOFT
numenta_81_cons_overall$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_numenta_81_fbiad, na.rm = TRUE))
numenta_81_cons_overall$f1_soft <- rbind(numenta_81_cons_overall$f1_soft, c(method = "ARIMA", value = mean(f1_soft_numenta_81_arima, na.rm = TRUE)))
numenta_81_cons_overall$f1_soft <- rbind(numenta_81_cons_overall$f1_soft, c(method = "GARCH", value = mean(f1_soft_numenta_81_garch, na.rm = TRUE)))
numenta_81_cons_overall$f1_soft <- rbind(numenta_81_cons_overall$f1_soft, c(method = "CF_LR", value = mean(f1_soft_numenta_81_cf_lr, na.rm = TRUE)))
numenta_81_cons_overall$f1_soft <- rbind(numenta_81_cons_overall$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_numenta_81_ml_lstm, na.rm = TRUE)))

##TIME
numenta_81_cons_overall$time_per_batch <- data.frame(method = "FBIAD", value = mean(time_per_batch_numenta_81_fbiad, na.rm = TRUE))
numenta_81_cons_overall$time_per_batch <- rbind(numenta_81_cons_overall$time_per_batch, c(method = "ARIMA", value = mean(time_per_batch_numenta_81_arima, na.rm = TRUE)))
numenta_81_cons_overall$time_per_batch <- rbind(numenta_81_cons_overall$time_per_batch, c(method = "GARCH", value = mean(time_per_batch_numenta_81_garch, na.rm = TRUE)))
numenta_81_cons_overall$time_per_batch <- rbind(numenta_81_cons_overall$time_per_batch, c(method = "CF_LR", value = mean(time_per_batch_numenta_81_cf_lr, na.rm = TRUE)))
numenta_81_cons_overall$time_per_batch <- rbind(numenta_81_cons_overall$time_per_batch, c(method = "ML_LSTM", value = mean(time_per_batch_numenta_81_ml_lstm, na.rm = TRUE)))

numenta_81_cons_overall
save(numenta_81_cons_overall, file = "~/janio/harbinger/dev/results/numenta_81_cons_overall.RData", compress = TRUE)
