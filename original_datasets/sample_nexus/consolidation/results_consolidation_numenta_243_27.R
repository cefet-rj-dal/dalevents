#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")

## art_anomaly -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_art_anom.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_art_anom.RData")
data(numenta_artificialWithAnomaly)

#Resume
numenta_243_27_cons <- list()
numenta_243_27_cons_overall <- list()


#FBIAD =========
ev_numenta_243_27_fbiad <- list()

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_art_anom[[k]]$detector,
             result_243_27_fbiad_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_243_27_fbiad <- list()

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_27_fbiad <- c() #All series to create overall results
numenta_243_27_cons$acc <- data.frame() #By Series


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                             c(method = "FBIAD_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_numenta_243_27_fbiad <- c() #All series to create overall results
numenta_243_27_cons$f1 <- data.frame() #By Series


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                             c(method = "FBIAD_ArtAnom",
                               value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_numenta_243_27_fbiad <- c() #All series to create overall results
numenta_243_27_cons$f1_soft <- data.frame() #By Series


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                 c(method = "FBIAD_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_per_batch_numenta_243_27_fbiad <- c() #All series
numenta_243_27_cons$time_per_batch <- data.frame()


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                          time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                        c(method = "FBIAD_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_art_anom)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_art_anom[[k]]$detector,
             result_243_27_arima_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_art_anom)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_27_arima <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                             c(method = "ARIMA_ArtAnom",
                               value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_numenta_243_27_arima <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                            c(method = "ARIMA_ArtAnom",
                              value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_numenta_243_27_arima <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                 c(method = "ARIMA_ArtAnom",
                                   value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_per_batch_numenta_243_27_arima <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                          time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                        c(method = "ARIMA_ArtAnom",
                                          value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_art_anom)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_art_anom[[k]]$detector,
             result_243_27_garch_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_art_anom)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_27_garch <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_ArtAnom",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_numenta_243_27_garch <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_ArtAnom",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_numenta_243_27_garch <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_ArtAnom",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_per_batch_numenta_243_27_garch <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_ArtAnom",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_art_anom)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_art_anom[[k]]$detector,
             result_243_27_cf_lr_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_art_anom)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_27_cf_lr <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_ArtAnom",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_numenta_243_27_cf_lr <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_ArtAnom",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_numenta_243_27_cf_lr <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_ArtAnom",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_per_batch_numenta_243_27_cf_lr <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_ArtAnom",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_art_anom)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_art_anom[[k]]$detector,
             result_243_27_ml_lstm_numenta_art_anom[[k]]$detection$event,
             numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_art_anom)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_art_anom[[k]]$detection$event),
             as.logical(numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_27_ml_lstm <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_ArtAnom",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_numenta_243_27_ml_lstm <- c() #All series to create overall results


f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_ArtAnom",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_numenta_243_27_ml_lstm <- c() #All series to create overall results


f1_soft_partial <- c()
j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_ArtAnom",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_per_batch_numenta_243_27_ml_lstm <- c() #All series


#ArtAnom
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_art_anom)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_art_anom[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_ArtAnom",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_art_anom)
rm(result_243_27_cf_lr_numenta_art_anom)
rm(result_243_27_fbiad_numenta_art_anom)
rm(result_243_27_garch_numenta_art_anom)
rm(result_243_27_ml_lstm_numenta_art_anom)
rm(numenta_artificialWithAnomaly)


## REAL AD EXC -----------------------------------------------------------
#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_real_adexc.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_real_adexc.RData")
result_243_27_ml_lstm_numenta_real_adexc <- result_243_27_ml_lstm_numenta_real #Adjusting wrong name
data(numenta_realAdExchange)

#FBIAD =========
ev_numenta_243_27_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_real_adexc[[k]]$detector,
             result_243_27_fbiad_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_243_27_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "FBIAD_realAdExch",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "FBIAD_realAdExch",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "FBIAD_realAdExch",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "FBIAD_realAdExch",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons

#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_adexc)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_real_adexc[[k]]$detector,
             result_243_27_arima_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_adexc)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ARIMA_realAdExch",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")



#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ARIMA_realAdExch",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")



#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ARIMA_realAdExch",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")


#Time
#realAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ARIMA_realAdExch",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_adexc)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_real_adexc[[k]]$detector,
             result_243_27_garch_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_adexc)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_realAdExch",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_realAdExch",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_realAdExch",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_realAdExch",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_adexc)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_real_adexc[[k]]$detector,
             result_243_27_cf_lr_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_adexc)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_realAdExch",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_realAdExch",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_realAdExch",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#realAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_realAdExch",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_adexc)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_real_adexc[[k]]$detector,
             result_243_27_ml_lstm_numenta_real_adexc[[k]]$detection$event,
             numenta_realAdExchange$realAdExchange[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_realAdExchange$realAdExchange)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_adexc)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_real_adexc[[k]]$detection$event),
             as.logical(numenta_realAdExchange$realAdExchange[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_realAdExch",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_realAdExch",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_realAdExch",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")


#Time
#realAdExch
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_real_adexc)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_real_adexc[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_realAdExch",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_real_adexc)
rm(result_243_27_cf_lr_numenta_real_adexc)
rm(result_243_27_fbiad_numenta_real_adexc)
rm(result_243_27_garch_numenta_real_adexc)
rm(result_243_27_ml_lstm_numenta_real_adexc)
rm(numenta_realAdExchange)


## REAL AWS -----------------------------------------------------------
#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_real_aws.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_real_aws.RData")
data(numenta_realAWSCloudwatch)

#FBIAD =========
ev_numenta_243_27_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_real_aws[[k]]$detector,
             result_243_27_fbiad_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_243_27_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)

## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "FBIAD_RealAWS",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc

#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "FBIAD_RealAWS",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "FBIAD_RealAWS",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "FBIAD_RealAWS",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_aws)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_real_aws[[k]]$detector,
             result_243_27_arima_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_aws)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ARIMA_RealAWS",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ARIMA_RealAWS",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ARIMA_RealAWS",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ARIMA_RealAWS",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_aws)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_real_aws[[k]]$detector,
             result_243_27_garch_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_aws)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_RealAWS",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_RealAWS",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_RealAWS",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_RealAWS",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_aws)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_real_aws[[k]]$detector,
             result_243_27_cf_lr_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_aws)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_RealAWS",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_RealAWS",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_RealAWS",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_RealAWS",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_aws)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_real_aws[[k]]$detector,
             result_243_27_ml_lstm_numenta_real_aws[[k]]$detection$event,
             numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_aws)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_real_aws[[k]]$detection$event),
             as.logical(numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_RealAWS",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_RealAWS",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_RealAWS",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealAWS
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_real_aws)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_real_aws[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_RealAWS",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_real_aws)
rm(result_243_27_cf_lr_numenta_real_aws)
rm(result_243_27_fbiad_numenta_real_aws)
rm(result_243_27_garch_numenta_real_aws)
rm(result_243_27_ml_lstm_numenta_real_aws)
rm(numenta_realAWSCloudwatch)



## REAL Known Cause ----------------------------------------------------
#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_real_knwcs.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_real_knwcs.RData")
data(numenta_realKnownCause)

#FBIAD =========
ev_numenta_243_27_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_real_knwcs[[k]]$detector,
             result_243_27_fbiad_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_243_27_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "FBIAD_RealKnownCause",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "FBIAD_RealKnownCause",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "FBIAD_RealKnownCause",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "FBIAD_RealKnownCause",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_knwcs)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_real_knwcs[[k]]$detector,
             result_243_27_arima_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_knwcs)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ARIMA_RealKnownCause",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ARIMA_RealKnownCause",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ARIMA_RealKnownCause",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ARIMA_RealKnownCause",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_knwcs)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_real_knwcs[[k]]$detector,
             result_243_27_garch_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_knwcs)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_RealKnownCause",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_RealKnownCause",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_RealKnownCause",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_RealKnownCause",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_knwcs)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_real_knwcs[[k]]$detector,
             result_243_27_cf_lr_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_knwcs)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_RealKnownCause",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_RealKnownCause",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_RealKnownCause",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_RealKnownCause",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_knwcs)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_real_knwcs[[k]]$detector,
             result_243_27_ml_lstm_numenta_real_knwcs[[k]]$detection$event,
             numenta_realKnownCause$realKnownCause[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_realKnownCause$realKnownCause)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_knwcs)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_real_knwcs[[k]]$detection$event),
             as.logical(numenta_realKnownCause$realKnownCause[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_RealKnownCause",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_RealKnownCause",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_RealKnownCause",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealKnownCause
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_real_knwcs)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_real_knwcs[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_RealKnownCause",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_real_knwcs)
rm(result_243_27_cf_lr_numenta_real_knwcs)
rm(result_243_27_fbiad_numenta_real_knwcs)
rm(result_243_27_garch_numenta_real_knwcs)
rm(result_243_27_ml_lstm_numenta_real_knwcs)
rm(numenta_realKnownCause)


## REAL Real Traffic ----------------------------------------------------
#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_real_traff.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_real_traff.RData")
data(numenta_realTraffic)


#FBIAD =========
ev_numenta_243_27_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_real_traff[[k]]$detector,
             result_243_27_fbiad_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_243_27_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "FBIAD_RealTraffic",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "FBIAD_RealTraffic",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "FBIAD_RealTraffic",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "FBIAD_RealTraffic",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_traff)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_real_traff[[k]]$detector,
             result_243_27_arima_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_traff)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ARIMA_RealTraffic",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ARIMA_RealTraffic",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ARIMA_RealTraffic",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ARIMA_RealTraffic",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_traff)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_real_traff[[k]]$detector,
             result_243_27_garch_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_traff)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_RealTraffic",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_RealTraffic",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_RealTraffic",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_RealTraffic",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_traff)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_real_traff[[k]]$detector,
             result_243_27_cf_lr_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_traff)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_RealTraffic",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_RealTraffic",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_RealTraffic",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_RealTraffic",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_traff)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_real_traff[[k]]$detector,
             result_243_27_ml_lstm_numenta_real_traff[[k]]$detection$event,
             numenta_realTraffic$realTraffic[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_realTraffic$realTraffic)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_traff)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_real_traff[[k]]$detection$event),
             as.logical(numenta_realTraffic$realTraffic[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_RealTraffic",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_RealTraffic",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_RealTraffic",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTraffic
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_real_traff)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_real_traff[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_RealTraffic",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_real_traff)
rm(result_243_27_cf_lr_numenta_real_traff)
rm(result_243_27_fbiad_numenta_real_traff)
rm(result_243_27_garch_numenta_real_traff)
rm(result_243_27_ml_lstm_numenta_real_traff)
rm(numenta_realTraffic)


## REAL Real Tweets ----------------------------------------------------
#Before using loaded data verify if it has the complete experiment results
#If not sure, use individual files instead of the complete experiment
load(file="~/janio/harbinger/dev/results/result_243_27_fbiad_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_arima_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_garch_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_cf_lr_numenta_real_tweets.RData")
load(file="~/janio/harbinger/dev/results/result_243_27_ml_lstm_numenta_real_tweets.RData")
data(numenta_realTweets)


#FBIAD =========
ev_numenta_243_27_fbiad <- list() #Restart EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)) {
  ev_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(result_243_27_fbiad_numenta_real_tweets[[k]]$detector,
             result_243_27_fbiad_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_fbiad) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_243_27_fbiad <- list() #Restart soft EV for each series group

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)) {
  ev_soft_numenta_243_27_fbiad[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_fbiad_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_fbiad) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_fbiad <- append(acc_numenta_243_27_fbiad, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "FBIAD_RealTweets",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_fbiad <- append(f1_numenta_243_27_fbiad, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "FBIAD_RealTweets",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_fbiad)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_fbiad <- append(f1_soft_numenta_243_27_fbiad, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "FBIAD_RealTweets",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_fbiad_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_fbiad_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_fbiad <- append(time_per_batch_numenta_243_27_fbiad,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "FBIAD_RealTweets",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ARIMA =========
ev_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_tweets)) {
  ev_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(result_243_27_arima_numenta_real_tweets[[k]]$detector,
             result_243_27_arima_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_arima) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_243_27_arima <- list()

k = 1
for (k in 1:length(result_243_27_arima_numenta_real_tweets)) {
  ev_soft_numenta_243_27_arima[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_arima_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_arima) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_arima <- append(acc_numenta_243_27_arima, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ARIMA_RealTweets",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_arima <- append(f1_numenta_243_27_arima, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ARIMA_RealTweets",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_arima)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_arima <- append(f1_soft_numenta_243_27_arima, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ARIMA_RealTweets",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_arima_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_arima_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_arima <- append(time_per_batch_numenta_243_27_arima,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ARIMA_RealTweets",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#GARCH =========
ev_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_tweets)) {
  ev_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(result_243_27_garch_numenta_real_tweets[[k]]$detector,
             result_243_27_garch_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_garch) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_243_27_garch <- list()

k = 1
for (k in 1:length(result_243_27_garch_numenta_real_tweets)) {
  ev_soft_numenta_243_27_garch[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_garch_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_garch) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_garch <- append(acc_numenta_243_27_garch, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "GARCH_RealTweets",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_garch <- append(f1_numenta_243_27_garch, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "GARCH_RealTweets",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_garch)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_garch <- append(f1_soft_numenta_243_27_garch, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "GARCH_RealTweets",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_garch_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_garch_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_garch <- append(time_per_batch_numenta_243_27_garch,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "GARCH_RealTweets",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#CF_LR =========
ev_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_tweets)) {
  ev_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(result_243_27_cf_lr_numenta_real_tweets[[k]]$detector,
             result_243_27_cf_lr_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_cf_lr) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_243_27_cf_lr <- list()

k = 1
for (k in 1:length(result_243_27_cf_lr_numenta_real_tweets)) {
  ev_soft_numenta_243_27_cf_lr[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_cf_lr_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_cf_lr) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_cf_lr <- append(acc_numenta_243_27_cf_lr, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "CF_LR_RealTweets",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_cf_lr <- append(f1_numenta_243_27_cf_lr, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "CF_LR_RealTweets",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_cf_lr)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_cf_lr <- append(f1_soft_numenta_243_27_cf_lr, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "CF_LR_RealTweets",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_cf_lr_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_cf_lr_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_cf_lr <- append(time_per_batch_numenta_243_27_cf_lr,
                                                time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "CF_LR_RealTweets",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


#ML_LSTM =========
ev_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_tweets)) {
  ev_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(result_243_27_ml_lstm_numenta_real_tweets[[k]]$detector,
             result_243_27_ml_lstm_numenta_real_tweets[[k]]$detection$event,
             numenta_realTweets$realTweets[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_numenta_243_27_ml_lstm) <- names(numenta_realTweets$realTweets)


ev_soft_numenta_243_27_ml_lstm <- list()

k = 1
for (k in 1:length(result_243_27_ml_lstm_numenta_real_tweets)) {
  ev_soft_numenta_243_27_ml_lstm[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_243_27_ml_lstm_numenta_real_tweets[[k]]$detection$event),
             as.logical(numenta_realTweets$realTweets[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_numenta_243_27_ml_lstm) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_27_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_27_ml_lstm <- append(acc_numenta_243_27_ml_lstm, acc_partial)
}

numenta_243_27_cons$acc <- rbind(numenta_243_27_cons$acc,
                                   c(method = "ML_LSTM_RealTweets",
                                     value = mean(acc_partial, na.rm = TRUE)))


names(numenta_243_27_cons$acc) <- c("method", "value")
numenta_243_27_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_27_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_27_ml_lstm <- append(f1_numenta_243_27_ml_lstm, f1_partial)
}

numenta_243_27_cons$f1 <- rbind(numenta_243_27_cons$f1,
                                  c(method = "ML_LSTM_RealTweets",
                                    value = mean(f1_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1) <- c("method", "value")
numenta_243_27_cons$f1


#F1_soft
f1_soft_partial <- c()

j = 1
for (j in 1:length(ev_soft_numenta_243_27_ml_lstm)) {
  f1_soft_partial <- tryCatch({
    append(f1_soft_partial, ev_soft_numenta_243_27_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_soft_partial, NA))
  })
  f1_soft_numenta_243_27_ml_lstm <- append(f1_soft_numenta_243_27_ml_lstm, f1_soft_partial)
}

numenta_243_27_cons$f1_soft <- rbind(numenta_243_27_cons$f1_soft,
                                       c(method = "ML_LSTM_RealTweets",
                                         value = mean(f1_soft_partial, na.rm = TRUE)))


names(numenta_243_27_cons$f1_soft) <- c("method", "value")
numenta_243_27_cons$f1_soft


#Time
#RealTweets
time_bt_partial <- c()

j = 1
for (j in 1:length(result_243_27_ml_lstm_numenta_real_tweets)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_243_27_ml_lstm_numenta_real_tweets[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}

#Add time to calculate the overall mean for all groups
time_per_batch_numenta_243_27_ml_lstm <- append(time_per_batch_numenta_243_27_ml_lstm,
                                                  time_bt_partial)


numenta_243_27_cons$time_per_batch <- rbind(numenta_243_27_cons$time_per_batch,
                                              c(method = "ML_LSTM_RealTweets",
                                                value = mean(time_bt_partial, na.rm = TRUE)))

names(numenta_243_27_cons$time_per_batch) <- c("method", "value")
numenta_243_27_cons


## Final tasks ===============
#Cleaning the results to manage session memory and avoid mistakes
rm(result_243_27_arima_numenta_real_tweets)
rm(result_243_27_cf_lr_numenta_real_tweets)
rm(result_243_27_fbiad_numenta_real_tweets)
rm(result_243_27_garch_numenta_real_tweets)
rm(result_243_27_ml_lstm_numenta_real_tweets)
rm(numenta_realTweets)


##=====
## Save final results
save(numenta_243_27_cons, file = "~/janio/harbinger/dev/results/numenta_243_27_cons.RData", compress = TRUE)
numenta_243_27_cons

##=====
##Calculate overall metrics (RUN AFTER COMPLETING ALL GROUPS)
numenta_243_27_cons_overall <- list()

##ACC
numenta_243_27_cons_overall$acc <- data.frame(method = "FBIAD", value = mean(acc_numenta_243_27_fbiad, na.rm = TRUE))
numenta_243_27_cons_overall$acc <- rbind(numenta_243_27_cons_overall$acc, c(method = "ARIMA", value = mean(acc_numenta_243_27_arima, na.rm = TRUE)))
numenta_243_27_cons_overall$acc <- rbind(numenta_243_27_cons_overall$acc, c(method = "GARCH", value = mean(acc_numenta_243_27_garch, na.rm = TRUE)))
numenta_243_27_cons_overall$acc <- rbind(numenta_243_27_cons_overall$acc, c(method = "CF_LR", value = mean(acc_numenta_243_27_cf_lr, na.rm = TRUE)))
numenta_243_27_cons_overall$acc <- rbind(numenta_243_27_cons_overall$acc, c(method = "ML_LSTM", value = mean(acc_numenta_243_27_ml_lstm, na.rm = TRUE)))

##F1
numenta_243_27_cons_overall$f1 <- data.frame(method = "FBIAD", value = mean(f1_numenta_243_27_fbiad, na.rm = TRUE))
numenta_243_27_cons_overall$f1 <- rbind(numenta_243_27_cons_overall$f1, c(method = "ARIMA", value = mean(f1_numenta_243_27_arima, na.rm = TRUE)))
numenta_243_27_cons_overall$f1 <- rbind(numenta_243_27_cons_overall$f1, c(method = "GARCH", value = mean(f1_numenta_243_27_garch, na.rm = TRUE)))
numenta_243_27_cons_overall$f1 <- rbind(numenta_243_27_cons_overall$f1, c(method = "CF_LR", value = mean(f1_numenta_243_27_cf_lr, na.rm = TRUE)))
numenta_243_27_cons_overall$f1 <- rbind(numenta_243_27_cons_overall$f1, c(method = "ML_LSTM", value = mean(f1_numenta_243_27_ml_lstm, na.rm = TRUE)))

##F1 SOFT
numenta_243_27_cons_overall$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_numenta_243_27_fbiad, na.rm = TRUE))
numenta_243_27_cons_overall$f1_soft <- rbind(numenta_243_27_cons_overall$f1_soft, c(method = "ARIMA", value = mean(f1_soft_numenta_243_27_arima, na.rm = TRUE)))
numenta_243_27_cons_overall$f1_soft <- rbind(numenta_243_27_cons_overall$f1_soft, c(method = "GARCH", value = mean(f1_soft_numenta_243_27_garch, na.rm = TRUE)))
numenta_243_27_cons_overall$f1_soft <- rbind(numenta_243_27_cons_overall$f1_soft, c(method = "CF_LR", value = mean(f1_soft_numenta_243_27_cf_lr, na.rm = TRUE)))
numenta_243_27_cons_overall$f1_soft <- rbind(numenta_243_27_cons_overall$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_numenta_243_27_ml_lstm, na.rm = TRUE)))

##TIME
numenta_243_27_cons_overall$time_per_batch <- data.frame(method = "FBIAD", value = mean(time_per_batch_numenta_243_27_fbiad, na.rm = TRUE))
numenta_243_27_cons_overall$time_per_batch <- rbind(numenta_243_27_cons_overall$time_per_batch, c(method = "ARIMA", value = mean(time_per_batch_numenta_243_27_arima, na.rm = TRUE)))
numenta_243_27_cons_overall$time_per_batch <- rbind(numenta_243_27_cons_overall$time_per_batch, c(method = "GARCH", value = mean(time_per_batch_numenta_243_27_garch, na.rm = TRUE)))
numenta_243_27_cons_overall$time_per_batch <- rbind(numenta_243_27_cons_overall$time_per_batch, c(method = "CF_LR", value = mean(time_per_batch_numenta_243_27_cf_lr, na.rm = TRUE)))
numenta_243_27_cons_overall$time_per_batch <- rbind(numenta_243_27_cons_overall$time_per_batch, c(method = "ML_LSTM", value = mean(time_per_batch_numenta_243_27_ml_lstm, na.rm = TRUE)))

numenta_243_27_cons_overall
save(numenta_243_27_cons_overall, file = "~/janio/harbinger/dev/results/numenta_243_27_cons_overall.RData", compress = TRUE)


## Stream Results ====================

#Stream Result complete
str_numenta_fbiad <- list()
str_numenta_arima <- list()
str_numenta_garch <- list()
str_numenta_cf <- list()
str_numenta_lstm <- list()

#Lag analysis
lag_numenta_fbiad <- c()
lag_numenta_arima <- c()
lag_numenta_garch <- c()
lag_numenta_cf <- c()
lag_numenta_lstm <- c()

lag_numenta_overall <- list()

## ART ANOM STR ============
str_numenta_fbiad$art_anom <- list()
str_numenta_arima$art_anom <- list()
str_numenta_garch$art_anom <- list()
str_numenta_cf$art_anom <- list()
str_numenta_lstm$art_anom <- list()


#FBIAD ====
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)){
  str_numenta_fbiad$art_anom[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_art_anom[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$art_anom[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$art_anom) <- names(result_243_27_fbiad_numenta_art_anom)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})


lag_numenta_overall$group <-data.frame(method = "FBIAD",
                                       group = "art_anom",
                                       mean = mean(lag_partial, na.rm = TRUE),
                                       median = median(lag_partial, na.rm = TRUE),
                                       max = max(lag_partial, na.rm = TRUE),
                                       min = min(lag_partial, na.rm = TRUE))


#ARIMA ====
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_arima_numenta_art_anom)){
  str_numenta_arima$art_anom[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_art_anom[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$art_anom[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$art_anom) <- names(result_243_27_arima_numenta_art_anom)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "art_anom",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#GARCH ====
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)){
  str_numenta_garch$art_anom[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_art_anom[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$art_anom[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$art_anom) <- names(result_243_27_garch_numenta_art_anom)


lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "art_anom",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF ====
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)){
  str_numenta_cf$art_anom[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_art_anom[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$art_anom[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$art_anom) <- names(result_243_27_cf_lr_numenta_art_anom)


lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "art_anom",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM ====
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_art_anom)){
  str_numenta_lstm$art_anom[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_art_anom[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$art_anom[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$art_anom) <- names(result_243_27_ml_lstm_numenta_art_anom)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "art_anom",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

lag_numenta_overall


## real_adexc STR ============
str_numenta_fbiad$real_adexc <- list()
str_numenta_arima$real_adexc <- list()
str_numenta_garch$real_adexc <- list()
str_numenta_cf$real_adexc <- list()
str_numenta_lstm$real_adexc <- list()


#FBIAD
lag_partial <- c() #Start partial

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)){
  
  str_numenta_fbiad$real_adexc[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_real_adexc[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$real_adexc[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$real_adexc) <- names(result_243_27_fbiad_numenta_real_adexc)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "FBIAD",
                                                                group = "real_adexc",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

#ARIMA
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)){
  str_numenta_arima$real_adexc[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_real_adexc[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$real_adexc[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$real_adexc) <- names(result_243_27_arima_numenta_real_adexc)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "real_adexc",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))
#GARCH
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)){
  str_numenta_garch$real_adexc[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_real_adexc[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$real_adexc[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$real_adexc) <- names(result_243_27_garch_numenta_real_adexc)


lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "real_adexc",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)){
  str_numenta_cf$real_adexc[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_real_adexc[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$real_adexc[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$real_adexc) <- names(result_243_27_cf_lr_numenta_real_adexc)

lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "real_adexc",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_adexc)){
  str_numenta_lstm$real_adexc[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_real_adexc[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$real_adexc[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$real_adexc) <- names(result_243_27_ml_lstm_numenta_real_adexc)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "real_adexc",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))



lag_numenta_overall


## real_aws STR ============
str_numenta_fbiad$real_aws <- list()
str_numenta_arima$real_aws <- list()
str_numenta_garch$real_aws <- list()
str_numenta_cf$real_aws <- list()
str_numenta_lstm$real_aws <- list()


#FBIAD
lag_partial <- c() #Start partial

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)){
  
  str_numenta_fbiad$real_aws[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_real_aws[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$real_aws[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$real_aws) <- names(result_243_27_fbiad_numenta_real_aws)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "FBIAD",
                                                                group = "real_aws",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

#ARIMA
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)){
  str_numenta_arima$real_aws[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_real_aws[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$real_aws[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$real_aws) <- names(result_243_27_arima_numenta_real_aws)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "real_aws",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))
#GARCH
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)){
  str_numenta_garch$real_aws[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_real_aws[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$real_aws[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$real_aws) <- names(result_243_27_garch_numenta_real_aws)


lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "real_aws",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)){
  str_numenta_cf$real_aws[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_real_aws[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$real_aws[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$real_aws) <- names(result_243_27_cf_lr_numenta_real_aws)

lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "real_aws",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_aws)){
  str_numenta_lstm$real_aws[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_real_aws[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$real_aws[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$real_aws) <- names(result_243_27_ml_lstm_numenta_real_aws)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "real_aws",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))



lag_numenta_overall


## real_knwcs STR ============
str_numenta_fbiad$real_knwcs <- list()
str_numenta_arima$real_knwcs <- list()
str_numenta_garch$real_knwcs <- list()
str_numenta_cf$real_knwcs <- list()
str_numenta_lstm$real_knwcs <- list()


#FBIAD
lag_partial <- c() #Start partial

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)){
  str_numenta_fbiad$real_knwcs[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_real_knwcs[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$real_knwcs[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$real_knwcs) <- names(result_243_27_fbiad_numenta_real_knwcs)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "FBIAD",
                                                                group = "real_knwcs",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

#ARIMA
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)){
  str_numenta_arima$real_knwcs[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_real_knwcs[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$real_knwcs[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$real_knwcs) <- names(result_243_27_arima_numenta_real_knwcs)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "real_knwcs",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))
#GARCH
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)){
  str_numenta_garch$real_knwcs[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_real_knwcs[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$real_knwcs[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$real_knwcs) <- names(result_243_27_garch_numenta_real_knwcs)

lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "real_knwcs",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)){
  str_numenta_cf$real_knwcs[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_real_knwcs[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$real_knwcs[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$real_knwcs) <- names(result_243_27_cf_lr_numenta_real_knwcs)

lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "real_knwcs",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_knwcs)){
  str_numenta_lstm$real_knwcs[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_real_knwcs[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$real_knwcs[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$real_knwcs) <- names(result_243_27_ml_lstm_numenta_real_knwcs)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "real_knwcs",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))



lag_numenta_overall



## real_traff STR ============
str_numenta_fbiad$real_traff <- list()
str_numenta_arima$real_traff <- list()
str_numenta_garch$real_traff <- list()
str_numenta_cf$real_traff <- list()
str_numenta_lstm$real_traff <- list()


#FBIAD
lag_partial <- c() #Start partial

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)){
  str_numenta_fbiad$real_traff[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_real_traff[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$real_traff[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$real_traff) <- names(result_243_27_fbiad_numenta_real_traff)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "FBIAD",
                                                                group = "real_traff",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

#ARIMA
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)){
  str_numenta_arima$real_traff[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_real_traff[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$real_traff[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$real_traff) <- names(result_243_27_arima_numenta_real_traff)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "real_traff",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))
#GARCH
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)){
  str_numenta_garch$real_traff[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_real_traff[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$real_traff[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$real_traff) <- names(result_243_27_garch_numenta_real_traff)


lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "real_traff",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)){
  str_numenta_cf$real_traff[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_real_traff[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$real_traff[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$real_traff) <- names(result_243_27_cf_lr_numenta_real_traff)

lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "real_traff",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_traff)){
  str_numenta_lstm$real_traff[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_real_traff[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$real_traff[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$real_traff) <- names(result_243_27_ml_lstm_numenta_real_traff)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "real_traff",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))



lag_numenta_overall

## real_tweets STR ============
str_numenta_fbiad$real_tweets <- list()
str_numenta_arima$real_tweets <- list()
str_numenta_garch$real_tweets <- list()
str_numenta_cf$real_tweets <- list()
str_numenta_lstm$real_tweets <- list()


#FBIAD
lag_partial <- c() #Start partial

k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)){
  str_numenta_fbiad$real_tweets[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_fbiad_numenta_real_tweets[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_fbiad$real_tweets[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_fbiad$real_tweets) <- names(result_243_27_fbiad_numenta_real_tweets)

lag_numenta_fbiad <- tryCatch({
  append(lag_numenta_fbiad, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_fbiad, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "FBIAD",
                                                                group = "real_tweets",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))

#ARIMA
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)){
  str_numenta_arima$real_tweets[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_arima_numenta_real_tweets[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_arima$real_tweets[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_arima$real_tweets) <- names(result_243_27_arima_numenta_real_tweets)

lag_numenta_arima <- tryCatch({
  append(lag_numenta_arima, lag_partial)
}, error = function(e) {
  append(lag_numenta_arima, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "ARIMA",
                                                                group = "real_tweets",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))
#GARCH
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)){
  str_numenta_garch$real_tweets[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_garch_numenta_real_tweets[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_garch$real_tweets[[k]]$lag)
  }, error = function(e){
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_garch$real_tweets) <- names(result_243_27_garch_numenta_real_tweets)

lag_numenta_garch <- tryCatch({
  append(lag_numenta_garch, lag_partial)
}, error = function(e){
  message(e)
  append(lag_numenta_garch, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "GARCH",
                                                                group = "real_tweets",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#CF
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)){
  str_numenta_cf$real_tweets[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_cf_lr_numenta_real_tweets[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_cf$real_tweets[[k]]$lag)
  }, error = function(e) {
    message(e)
    append(lag_partial, NA)
  })
}
names(str_numenta_cf$real_tweets) <- names(result_243_27_cf_lr_numenta_real_tweets)

lag_numenta_cf <- tryCatch({
  append(lag_numenta_cf, lag_partial)
}, error = function(e) {
  message(e)
  append(lag_numenta_cf, NA)
})

lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "CF",
                                                                group = "real_tweets",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))


#LSTM
lag_partial <- c() #Start partial
k = 1
for (k in 1:length(result_243_27_fbiad_numenta_real_tweets)){
  str_numenta_lstm$real_tweets[[k]] <- tryCatch({
    stream_evaluate(nexus_result = result_243_27_ml_lstm_numenta_real_tweets[[k]])
  }, error = function(e) {
    message(e)
    return(NA)
  })
  lag_partial <- tryCatch({
    append(lag_partial, str_numenta_lstm$real_tweets[[k]]$lag)
  }, error = function(e) {
    append(lag_partial, NA)
  })
}
names(str_numenta_lstm$real_tweets) <- names(result_243_27_ml_lstm_numenta_real_tweets)

lag_numenta_lstm <- tryCatch({
  append(lag_numenta_lstm, lag_partial)
}, error = function(e) {
  append(lag_numenta_lstm, NA)
})

#Str group 
lag_numenta_overall$group <- rbind(lag_numenta_overall$group, c(method = "LSTM",
                                                                group = "real_tweets",
                                                                mean = mean(lag_partial, na.rm = TRUE),
                                                                median = median(lag_partial, na.rm = TRUE),
                                                                max = max(lag_partial, na.rm = TRUE),
                                                                min = min(lag_partial, na.rm = TRUE)))



lag_numenta_overall


#Consolidations ==========
lag_numenta_overall$complete <-data.frame(method = "FBIAD",
                                          mean = mean(lag_numenta_fbiad, na.rm = TRUE),
                                          median = median(lag_numenta_fbiad, na.rm = TRUE),
                                          max = max(lag_numenta_fbiad, na.rm = TRUE),
                                          min = min(lag_numenta_fbiad, na.rm = TRUE))

lag_numenta_overall$complete <- rbind(lag_numenta_overall$complete, c(method = "ARIMA",
                                                                      mean = mean(lag_numenta_arima, na.rm = TRUE),
                                                                      median = median(lag_numenta_arima, na.rm = TRUE),
                                                                      max = max(lag_numenta_arima, na.rm = TRUE),
                                                                      min = min(lag_numenta_arima, na.rm = TRUE)))

lag_numenta_overall$complete <- rbind(lag_numenta_overall$complete, c(method = "GARCH",
                                                                      mean = mean(lag_numenta_garch, na.rm = TRUE),
                                                                      median = median(lag_numenta_garch, na.rm = TRUE),
                                                                      max = max(lag_numenta_garch, na.rm = TRUE),
                                                                      min = min(lag_numenta_garch, na.rm = TRUE)))

lag_numenta_overall$complete <- rbind(lag_numenta_overall$complete, c(method = "CF",
                                                                      mean = mean(lag_numenta_cf, na.rm = TRUE),
                                                                      median = median(lag_numenta_cf, na.rm = TRUE),
                                                                      max = max(lag_numenta_cf, na.rm = TRUE),
                                                                      min = min(lag_numenta_cf, na.rm = TRUE)))

lag_numenta_overall$complete <- rbind(lag_numenta_overall$complete, c(method = "LSTM",
                                                                      mean = mean(lag_numenta_lstm, na.rm = TRUE),
                                                                      median = median(lag_numenta_lstm, na.rm = TRUE),
                                                                      max = max(lag_numenta_lstm, na.rm = TRUE),
                                                                      min = min(lag_numenta_lstm, na.rm = TRUE)))



lag_numenta_overall
save(lag_numenta_overall, file = "~/janio/harbinger/dev/results/lag_numenta_overall.RData", compress = TRUE)
