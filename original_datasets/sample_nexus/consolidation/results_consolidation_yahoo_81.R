#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")


# YAHOO --------------------------------------------------------------------
data(yahoo_a1)
data(yahoo_a2)
data(yahoo_a3)
data(yahoo_a4)

## yahoo_81 -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
#FBIAD RESULTS
load("~/janio/harbinger/dev/results/result_81_fbiad_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_81_fbiad_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_81_fbiad_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_81_fbiad_yh_a4_complete.RData")

#ARIMA RESULTS
load("~/janio/harbinger/dev/results/result_81_arima_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_81_arima_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_81_arima_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_81_arima_yh_a4_complete.RData")

#GARCH RESULTS
load("~/janio/harbinger/dev/results/result_81_garch_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_81_garch_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_81_garch_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_81_garch_yh_a4_complete.RData")

#CF_LR RESULTS
load("~/janio/harbinger/dev/results/result_81_cf_lr_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_81_cf_lr_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_81_cf_lr_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_81_cf_lr_yh_a4_complete.RData")


#ML_LSTM RESULTS
load("~/janio/harbinger/dev/results/result_81_ml_lstm_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_81_ml_lstm_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_81_ml_lstm_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_81_ml_lstm_yh_a4_complete.RData")

#Resume
yahoo_81_cons <- list()
yahoo_81_cons_overall <- list()


#FBIAD =========
ev_yahoo_81_fbiad <- list()

#A1
k = 1
for (k in 1:length(result_81_fbiad_yh_a1)) {
  ev_yahoo_81_fbiad$a1[[k]] <- tryCatch({
    evaluate(result_81_fbiad_yh_a1[[k]]$detector,
             result_81_fbiad_yh_a1[[k]]$detection$event,
             yahoo_a1[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_fbiad$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_fbiad_yh_a2)) {
  ev_yahoo_81_fbiad$a2[[k]] <- tryCatch({
    evaluate(result_81_fbiad_yh_a2[[k]]$detector,
             result_81_fbiad_yh_a2[[k]]$detection$event,
             yahoo_a2[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_fbiad$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_fbiad_yh_a3)) {
  ev_yahoo_81_fbiad$a3[[k]] <- tryCatch({
    evaluate(result_81_fbiad_yh_a3[[k]]$detector,
             result_81_fbiad_yh_a3[[k]]$detection$event,
             yahoo_a3[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_fbiad$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_fbiad_yh_a4)) {
  ev_yahoo_81_fbiad$a4[[k]] <- tryCatch({
    evaluate(result_81_fbiad_yh_a4[[k]]$detector,
             result_81_fbiad_yh_a4[[k]]$detection$event,
             yahoo_a4[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_fbiad$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_81_fbiad <- c()
yahoo_81_cons$acc <- data.frame()

j = 1


for (j in 1:length(ev_yahoo_81_fbiad)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_fbiad[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, ev_yahoo_81_fbiad[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  yahoo_81_cons$acc <- rbind(yahoo_81_cons$acc, c(method = paste("FBIAD_A", j, sep = ""), value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_81_fbiad <- append(acc_yahoo_81_fbiad, acc_partial)
}
names(yahoo_81_cons$acc) <- c("method", "value")

yahoo_81_cons_overall$acc <- data.frame(method = "FBIAD", value = mean(acc_yahoo_81_fbiad, na.rm = TRUE))


yahoo_81_cons
yahoo_81_cons_overall

#F1
f1_yahoo_81_fbiad <- c()
yahoo_81_cons$f1 <- data.frame()


j = 1


for (j in 1:length(ev_yahoo_81_fbiad)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_fbiad[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_yahoo_81_fbiad[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1 <- rbind(yahoo_81_cons$f1, c(method = paste("FBIAD_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_81_fbiad <- append(f1_yahoo_81_fbiad, f1_partial)
}

names(yahoo_81_cons$f1) <- c("method", "value")

yahoo_81_cons_overall$f1 <- data.frame(method = "FBIAD", value = mean(f1_yahoo_81_fbiad, na.rm = TRUE))


yahoo_81_cons
yahoo_81_cons_overall



#Time
result_81_fbiad_yh_a1[[1]]$time
length(diff(result_81_fbiad_yh_a1[[1]]$time))

time_per_batch_yahoo_81_fbiad <- c()
yahoo_81_cons$time_per_batch <- data.frame()
yahoo_81_cons_overall$time_per_batch <- data.frame()

#A1
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_yh_a1)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_yh_a1[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_fbiad <- append(time_per_batch_yahoo_81_fbiad,
                                        time_bt_partial)
time_per_batch_yahoo_81_fbiad 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- data.frame(method = "FBIAD_A1",
                                           value = mean(time_bt_partial, na.rm = TRUE))

yahoo_81_cons
yahoo_81_cons_overall


#A2
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_yh_a2)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_yh_a2[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_fbiad <- append(time_per_batch_yahoo_81_fbiad,
                                        time_bt_partial)
time_per_batch_yahoo_81_fbiad 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "FBIAD_A2", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A3
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_yh_a3)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_yh_a3[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_fbiad <- append(time_per_batch_yahoo_81_fbiad,
                                        time_bt_partial)
time_per_batch_yahoo_81_fbiad 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "FBIAD_A3", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A4
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_fbiad_yh_a4)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_fbiad_yh_a4[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_fbiad <- append(time_per_batch_yahoo_81_fbiad,
                                        time_bt_partial)
time_per_batch_yahoo_81_fbiad 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "FBIAD_A4", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons


yahoo_81_cons_overall$time_per_batch <- rbind(yahoo_81_cons_overall$time_per_batch,
                                              c(method = "FBIAD", value = mean(time_per_batch_yahoo_81_fbiad, na.rm = TRUE)))

names(yahoo_81_cons_overall$time_per_batch) <- c("method", "value")
yahoo_81_cons_overall


#arima =================
ev_yahoo_81_arima <- list()

#A1
k = 1
for (k in 1:length(result_81_arima_yh_a1)) {
  ev_yahoo_81_arima$a1[[k]] <- tryCatch({
    evaluate(result_81_arima_yh_a1[[k]]$detector,
             result_81_arima_yh_a1[[k]]$detection$event,
             yahoo_a1[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_arima$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_arima_yh_a2)) {
  ev_yahoo_81_arima$a2[[k]] <- tryCatch({
    evaluate(result_81_arima_yh_a2[[k]]$detector,
             result_81_arima_yh_a2[[k]]$detection$event,
             yahoo_a2[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_arima$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_arima_yh_a3)) {
  ev_yahoo_81_arima$a3[[k]] <- tryCatch({
    evaluate(result_81_arima_yh_a3[[k]]$detector,
             result_81_arima_yh_a3[[k]]$detection$event,
             yahoo_a3[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_arima$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_arima_yh_a4)) {
  ev_yahoo_81_arima$a4[[k]] <- tryCatch({
    evaluate(result_81_arima_yh_a4[[k]]$detector,
             result_81_arima_yh_a4[[k]]$detection$event,
             yahoo_a4[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_arima$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_81_arima <- c()

j = 1


for (j in 1:length(ev_yahoo_81_arima)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_arima[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, ev_yahoo_81_arima[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  yahoo_81_cons$acc <- rbind(yahoo_81_cons$acc, c(method = paste("ARIMA_A", j, sep = ""), value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_81_arima <- append(acc_yahoo_81_arima, acc_partial)
}
names(yahoo_81_cons$acc) <- c("method", "value")

yahoo_81_cons_overall$acc <- rbind(yahoo_81_cons_overall$acc, c(method = "ARIMA", value = mean(acc_yahoo_81_arima, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall

#F1
f1_yahoo_81_arima <- c()

j = 1


for (j in 1:length(ev_yahoo_81_arima)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_arima[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_yahoo_81_arima[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1 <- rbind(yahoo_81_cons$f1, c(method = paste("ARIMA_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_81_arima <- append(f1_yahoo_81_arima, f1_partial)
}

names(yahoo_81_cons$f1) <- c("method", "value")

yahoo_81_cons_overall$f1 <- rbind(yahoo_81_cons_overall$f1, c(method = "ARIMA", value = mean(f1_yahoo_81_arima, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall



#Time
time_per_batch_yahoo_81_arima <- c()

#A1
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_yh_a1)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_yh_a1[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_arima <- append(time_per_batch_yahoo_81_arima,
                                        time_bt_partial)
time_per_batch_yahoo_81_arima 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ARIMA_A1",value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A2
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_yh_a2)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_yh_a2[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_arima <- append(time_per_batch_yahoo_81_arima,
                                        time_bt_partial)
time_per_batch_yahoo_81_arima 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ARIMA_A2", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A3
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_yh_a3)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_yh_a3[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_arima <- append(time_per_batch_yahoo_81_arima,
                                        time_bt_partial)
time_per_batch_yahoo_81_arima 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ARIMA_A3", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A4
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_arima_yh_a4)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_arima_yh_a4[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_arima <- append(time_per_batch_yahoo_81_arima,
                                        time_bt_partial)
time_per_batch_yahoo_81_arima 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ARIMA_A4", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons


yahoo_81_cons_overall$time_per_batch <- rbind(yahoo_81_cons_overall$time_per_batch,
                                              c(method = "ARIMA", value = mean(time_per_batch_yahoo_81_arima, na.rm = TRUE)))

names(yahoo_81_cons_overall$time_per_batch) <- c("method", "value")
yahoo_81_cons_overall


## GARCH ##============================
ev_yahoo_81_garch <- list()

#A1
k = 1
for (k in 1:length(result_81_garch_yh_a1)) {
  ev_yahoo_81_garch$a1[[k]] <- tryCatch({
    evaluate(result_81_garch_yh_a1[[k]]$detector,
             result_81_garch_yh_a1[[k]]$detection$event,
             yahoo_a1[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_garch$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_garch_yh_a2)) {
  ev_yahoo_81_garch$a2[[k]] <- tryCatch({
    evaluate(result_81_garch_yh_a2[[k]]$detector,
             result_81_garch_yh_a2[[k]]$detection$event,
             yahoo_a2[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_garch$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_garch_yh_a3)) {
  ev_yahoo_81_garch$a3[[k]] <- tryCatch({
    evaluate(result_81_garch_yh_a3[[k]]$detector,
             result_81_garch_yh_a3[[k]]$detection$event,
             yahoo_a3[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_garch$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_garch_yh_a4)) {
  ev_yahoo_81_garch$a4[[k]] <- tryCatch({
    evaluate(result_81_garch_yh_a4[[k]]$detector,
             result_81_garch_yh_a4[[k]]$detection$event,
             yahoo_a4[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_garch$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_81_garch <- c()

j = 1


for (j in 1:length(ev_yahoo_81_garch)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_garch[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, ev_yahoo_81_garch[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  yahoo_81_cons$acc <- rbind(yahoo_81_cons$acc, c(method = paste("GARCH_A", j, sep = ""), value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_81_garch <- append(acc_yahoo_81_garch, acc_partial)
}
names(yahoo_81_cons$acc) <- c("method", "value")

yahoo_81_cons_overall$acc <- rbind(yahoo_81_cons_overall$acc, c(method = "GARCH", value = mean(acc_yahoo_81_garch, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall

#F1
f1_yahoo_81_garch <- c()

j = 1


for (j in 1:length(ev_yahoo_81_garch)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_garch[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_yahoo_81_garch[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1 <- rbind(yahoo_81_cons$f1, c(method = paste("GARCH_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_81_garch <- append(f1_yahoo_81_garch, f1_partial)
}

names(yahoo_81_cons$f1) <- c("method", "value")

yahoo_81_cons_overall$f1 <- rbind(yahoo_81_cons_overall$f1, c(method = "GARCH", value = mean(f1_yahoo_81_garch, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall



#Time
time_per_batch_yahoo_81_garch <- c()

#A1
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_yh_a1)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_yh_a1[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_garch <- append(time_per_batch_yahoo_81_garch,
                                        time_bt_partial)
time_per_batch_yahoo_81_garch 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "GARCH_A1",value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A2
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_yh_a2)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_yh_a2[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_garch <- append(time_per_batch_yahoo_81_garch,
                                        time_bt_partial)
time_per_batch_yahoo_81_garch 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "GARCH_A2", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A3
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_yh_a3)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_yh_a3[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_garch <- append(time_per_batch_yahoo_81_garch,
                                        time_bt_partial)
time_per_batch_yahoo_81_garch 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "GARCH_A3", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A4
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_garch_yh_a4)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_garch_yh_a4[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_garch <- append(time_per_batch_yahoo_81_garch,
                                        time_bt_partial)
time_per_batch_yahoo_81_garch 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "GARCH_A4", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons


yahoo_81_cons_overall$time_per_batch <- rbind(yahoo_81_cons_overall$time_per_batch,
                                              c(method = "GARCH", value = mean(time_per_batch_yahoo_81_garch, na.rm = TRUE)))

names(yahoo_81_cons_overall$time_per_batch) <- c("method", "value")
yahoo_81_cons_overall


## CF ##=======================================
ev_yahoo_81_cf_lr <- list()

#A1
k = 1
for (k in 1:length(result_81_cf_lr_yh_a1)) {
  ev_yahoo_81_cf_lr$a1[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_yh_a1[[k]]$detector,
             result_81_cf_lr_yh_a1[[k]]$detection$event,
             yahoo_a1[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_cf_lr$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_cf_lr_yh_a2)) {
  ev_yahoo_81_cf_lr$a2[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_yh_a2[[k]]$detector,
             result_81_cf_lr_yh_a2[[k]]$detection$event,
             yahoo_a2[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_cf_lr$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_cf_lr_yh_a3)) {
  ev_yahoo_81_cf_lr$a3[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_yh_a3[[k]]$detector,
             result_81_cf_lr_yh_a3[[k]]$detection$event,
             yahoo_a3[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_cf_lr$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_cf_lr_yh_a4)) {
  ev_yahoo_81_cf_lr$a4[[k]] <- tryCatch({
    evaluate(result_81_cf_lr_yh_a4[[k]]$detector,
             result_81_cf_lr_yh_a4[[k]]$detection$event,
             yahoo_a4[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_cf_lr$a4) <- names(yahoo_a4)


#METHOD DOES NOT WORK IN THIS SERIES

## METRICS
#Accuracy
j = 1


for (j in 1:length(ev_yahoo_81_cf_lr)) {
  yahoo_81_cons$acc <- rbind(yahoo_81_cons$acc, c(method = paste("CF_LR_A", j, sep = ""), value = NA))
}
names(yahoo_81_cons$acc) <- c("method", "value")

yahoo_81_cons_overall$acc <- rbind(yahoo_81_cons_overall$acc, c(method = "CF_LR", value = NA))


yahoo_81_cons
yahoo_81_cons_overall

#F1
j = 1

for (j in 1:length(ev_yahoo_81_cf_lr)) {
  yahoo_81_cons$f1 <- rbind(yahoo_81_cons$f1, c(method = paste("CF_LR_A", j, sep = ""), value = NA))
}

names(yahoo_81_cons$f1) <- c("method", "value")

yahoo_81_cons_overall$f1 <- rbind(yahoo_81_cons_overall$f1, c(method = "CF_LR", value = NA))


yahoo_81_cons
yahoo_81_cons_overall



#Time
j = 1

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "cf_lr_A1",value = NA))

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "cf_lr_A2", value = NA))

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "cf_lr_A3", value = NA))

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "cf_lr_A4", NA))

yahoo_81_cons_overall$time_per_batch <- rbind(yahoo_81_cons_overall$time_per_batch,
                                              c(method = "CF_LR", NA))

names(yahoo_81_cons_overall$time_per_batch) <- c("method", "value")

yahoo_81_cons
yahoo_81_cons_overall


## LSTM ##=====================
ev_yahoo_81_ml_lstm <- list()

#A1
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a1)) {
  ev_yahoo_81_ml_lstm$a1[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_yh_a1[[k]]$detector,
             result_81_ml_lstm_yh_a1[[k]]$detection$event,
             yahoo_a1[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_ml_lstm$a1) <- names(yahoo_a1)


ev <- evaluate(har_eval_soft(),
               as.logical(result_81_ml_lstm_yh_a1[[1]]$detection$event),
               as.logical(yahoo_a1[[1]]$event))




#A2
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a2)) {
  ev_yahoo_81_ml_lstm$a2[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_yh_a2[[k]]$detector,
             result_81_ml_lstm_yh_a2[[k]]$detection$event,
             yahoo_a2[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_ml_lstm$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a3)) {
  ev_yahoo_81_ml_lstm$a3[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_yh_a3[[k]]$detector,
             result_81_ml_lstm_yh_a3[[k]]$detection$event,
             yahoo_a3[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_ml_lstm$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a4)) {
  ev_yahoo_81_ml_lstm$a4[[k]] <- tryCatch({
    evaluate(result_81_ml_lstm_yh_a4[[k]]$detector,
             result_81_ml_lstm_yh_a4[[k]]$detection$event,
             yahoo_a4[[k]]$event)
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_yahoo_81_ml_lstm$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_81_ml_lstm <- c()

j = 1


for (j in 1:length(ev_yahoo_81_ml_lstm)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_ml_lstm[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, ev_yahoo_81_ml_lstm[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  yahoo_81_cons$acc <- rbind(yahoo_81_cons$acc, c(method = paste("ML_LSTM_A", j, sep = ""), value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_81_ml_lstm <- append(acc_yahoo_81_ml_lstm, acc_partial)
}
names(yahoo_81_cons$acc) <- c("method", "value")

yahoo_81_cons_overall$acc <- rbind(yahoo_81_cons_overall$acc, c(method = "ML_LSTM", value = mean(acc_yahoo_81_ml_lstm, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall

#F1
f1_yahoo_81_ml_lstm <- c()

j = 1


for (j in 1:length(ev_yahoo_81_ml_lstm)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_yahoo_81_ml_lstm[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_yahoo_81_ml_lstm[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1 <- rbind(yahoo_81_cons$f1, c(method = paste("ML_LSTM_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_81_ml_lstm <- append(f1_yahoo_81_ml_lstm, f1_partial)
}

names(yahoo_81_cons$f1) <- c("method", "value")

yahoo_81_cons_overall$f1 <- rbind(yahoo_81_cons_overall$f1, c(method = "ML_LSTM", value = mean(f1_yahoo_81_ml_lstm, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall



#Time
time_per_batch_yahoo_81_ml_lstm <- c()

#A1
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_yh_a1)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_yh_a1[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_ml_lstm <- append(time_per_batch_yahoo_81_ml_lstm,
                                          time_bt_partial)
time_per_batch_yahoo_81_ml_lstm 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ML_LSTM_A1",value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A2
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_yh_a2)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_yh_a2[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_ml_lstm <- append(time_per_batch_yahoo_81_ml_lstm,
                                          time_bt_partial)
time_per_batch_yahoo_81_ml_lstm 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ML_LSTM_A2", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A3
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_yh_a3)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_yh_a3[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_ml_lstm <- append(time_per_batch_yahoo_81_ml_lstm,
                                          time_bt_partial)
time_per_batch_yahoo_81_ml_lstm 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ML_LSTM_A3", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons
yahoo_81_cons_overall


#A4
time_bt_partial <- c()

j = 1
for (j in 1:length(result_81_ml_lstm_yh_a4)) {
  time_bt_partial <- tryCatch({
    append(time_bt_partial, diff(result_81_ml_lstm_yh_a4[[j]]$time))
  }, error = function(e) {
    message(e)
    return(append(time_bt_partial, NA))
  })
}


#Adicionar tempos para cálculo da média geral de todos os grupos
time_per_batch_yahoo_81_ml_lstm <- append(time_per_batch_yahoo_81_ml_lstm,
                                          time_bt_partial)
time_per_batch_yahoo_81_ml_lstm 

#Cálculo da métrica do grupo
yahoo_81_cons$time_per_batch <- rbind(yahoo_81_cons$time_per_batch,
                                      c(method = "ML_LSTM_A4", value = mean(time_bt_partial, na.rm = TRUE)))

yahoo_81_cons


yahoo_81_cons_overall$time_per_batch <- rbind(yahoo_81_cons_overall$time_per_batch,
                                              c(method = "ML_LSTM", value = mean(time_per_batch_yahoo_81_ml_lstm, na.rm = TRUE)))

names(yahoo_81_cons_overall$time_per_batch) <- c("method", "value")
yahoo_81_cons_overall

##=====
## Save final results
save(yahoo_81_cons, file = "~/janio/harbinger/dev/results/yahoo_81_cons.RData", compress = TRUE)
save(yahoo_81_cons_overall, file = "~/janio/harbinger/dev/results/yahoo_81_cons_overall.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/yahoo_81_cons.RData")
yahoo_81_cons

load("~/janio/harbinger/dev/results/yahoo_81_cons_overall.RData")
yahoo_81_cons_overall


#Soft Metrics
#w = 81, s = 81, m = 0
#FBIAD
ev_soft_yahoo_81_fbiad <- list()

#A1
k = 1
for (k in 1:length(result_81_fbiad_yh_a1)) {
  ev_soft_yahoo_81_fbiad$a1[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_yh_a1[[k]]$detection$event),
             as.logical(yahoo_a1[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_fbiad$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_fbiad_yh_a2)) {
  ev_soft_yahoo_81_fbiad$a2[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_yh_a2[[k]]$detection$event),
             as.logical(yahoo_a2[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_fbiad$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_fbiad_yh_a3)) {
  ev_soft_yahoo_81_fbiad$a3[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_yh_a3[[k]]$detection$event),
             as.logical(yahoo_a3[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_fbiad$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_fbiad_yh_a4)) {
  ev_soft_yahoo_81_fbiad$a4[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_fbiad_yh_a4[[k]]$detection$event),
             as.logical(yahoo_a4[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_fbiad$a4) <- names(yahoo_a4)
ev_soft_yahoo_81_fbiad


## METRICS
#F1_softED
f1_soft_yahoo_81_fbiad <- c()
yahoo_81_cons$f1_soft <- data.frame()


j = 1
for (j in 1:length(ev_soft_yahoo_81_fbiad)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_soft_yahoo_81_fbiad[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_soft_yahoo_81_fbiad[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1_soft <- rbind(yahoo_81_cons$f1_soft, c(method = paste("FBIAD_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_soft_yahoo_81_fbiad <- append(f1_soft_yahoo_81_fbiad, f1_partial)
}

names(yahoo_81_cons$f1_soft) <- c("method", "value")

yahoo_81_cons_overall$f1_soft <- data.frame(method = "FBIAD", value = mean(f1_soft_yahoo_81_fbiad, na.rm = TRUE))

yahoo_81_cons
yahoo_81_cons_overall


#ARIMA
ev_soft_yahoo_81_arima <- list()

#A1
k = 1
for (k in 1:length(result_81_arima_yh_a1)) {
  ev_soft_yahoo_81_arima$a1[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_yh_a1[[k]]$detection$event),
             as.logical(yahoo_a1[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_arima$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_arima_yh_a2)) {
  ev_soft_yahoo_81_arima$a2[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_yh_a2[[k]]$detection$event),
             as.logical(yahoo_a2[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_arima$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_arima_yh_a3)) {
  ev_soft_yahoo_81_arima$a3[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_yh_a3[[k]]$detection$event),
             as.logical(yahoo_a3[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_arima$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_arima_yh_a4)) {
  ev_soft_yahoo_81_arima$a4[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_arima_yh_a4[[k]]$detection$event),
             as.logical(yahoo_a4[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_arima$a4) <- names(yahoo_a4)

ev_soft_yahoo_81_arima


## METRICS
#F1_softED
f1_soft_yahoo_81_arima <- c()

j = 1
for (j in 1:length(ev_soft_yahoo_81_arima)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_soft_yahoo_81_arima[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_soft_yahoo_81_arima[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1_soft <- rbind(yahoo_81_cons$f1_soft, c(method = paste("ARIMA_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_soft_yahoo_81_arima <- append(f1_soft_yahoo_81_arima, f1_partial)
}

names(yahoo_81_cons$f1_soft) <- c("method", "value")

yahoo_81_cons_overall$f1_soft <- rbind(yahoo_81_cons_overall$f1_soft, c(method = "ARIMA", value = mean(f1_soft_yahoo_81_arima, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall


#GARCH
ev_soft_yahoo_81_garch <- list()

#A1
k = 1
for (k in 1:length(result_81_garch_yh_a1)) {
  ev_soft_yahoo_81_garch$a1[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_yh_a1[[k]]$detection$event),
             as.logical(yahoo_a1[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_garch$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_garch_yh_a2)) {
  ev_soft_yahoo_81_garch$a2[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_yh_a2[[k]]$detection$event),
             as.logical(yahoo_a2[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_garch$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_garch_yh_a3)) {
  ev_soft_yahoo_81_garch$a3[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_yh_a3[[k]]$detection$event),
             as.logical(yahoo_a3[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_garch$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_garch_yh_a4)) {
  ev_soft_yahoo_81_garch$a4[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_garch_yh_a4[[k]]$detection$event),
             as.logical(yahoo_a4[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_garch$a4) <- names(yahoo_a4)

ev_soft_yahoo_81_garch


## METRICS
#F1_softED
f1_soft_yahoo_81_garch <- c()

j = 1
for (j in 1:length(ev_soft_yahoo_81_garch)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_soft_yahoo_81_garch[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_soft_yahoo_81_garch[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1_soft <- rbind(yahoo_81_cons$f1_soft, c(method = paste("GARCH_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_soft_yahoo_81_garch <- append(f1_soft_yahoo_81_garch, f1_partial)
}

names(yahoo_81_cons$f1_soft) <- c("method", "value")

yahoo_81_cons_overall$f1_soft <- rbind(yahoo_81_cons_overall$f1_soft, c(method = "GARCH", value = mean(f1_soft_yahoo_81_garch, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall

#CF
ev_soft_yahoo_81_cf_lr <- list()

#A1
k = 1
for (k in 1:length(result_81_cf_lr_yh_a1)) {
  ev_soft_yahoo_81_cf_lr$a1[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_yh_a1[[k]]$detection$event),
             as.logical(yahoo_a1[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_cf_lr$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_cf_lr_yh_a2)) {
  ev_soft_yahoo_81_cf_lr$a2[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_yh_a2[[k]]$detection$event),
             as.logical(yahoo_a2[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_cf_lr$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_cf_lr_yh_a3)) {
  ev_soft_yahoo_81_cf_lr$a3[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_yh_a3[[k]]$detection$event),
             as.logical(yahoo_a3[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_cf_lr$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_cf_lr_yh_a4)) {
  ev_soft_yahoo_81_cf_lr$a4[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_cf_lr_yh_a4[[k]]$detection$event),
             as.logical(yahoo_a4[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_cf_lr$a4) <- names(yahoo_a4)

ev_soft_yahoo_81_cf_lr


## METRICS
#F1_softED
f1_soft_yahoo_81_cf_lr <- c()

j = 1
for (j in 1:length(ev_soft_yahoo_81_cf_lr)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_soft_yahoo_81_cf_lr[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_soft_yahoo_81_cf_lr[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1_soft <- rbind(yahoo_81_cons$f1_soft, c(method = paste("CF_LR_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_soft_yahoo_81_cf_lr <- append(f1_soft_yahoo_81_cf_lr, f1_partial)
}

names(yahoo_81_cons$f1_soft) <- c("method", "value")

yahoo_81_cons_overall$f1_soft <- rbind(yahoo_81_cons_overall$f1_soft, c(method = "CF_LR", value = mean(f1_soft_yahoo_81_cf_lr, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall

#LSTM
ev_soft_yahoo_81_ml_lstm <- list()

#A1
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a1)) {
  ev_soft_yahoo_81_ml_lstm$a1[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_yh_a1[[k]]$detection$event),
             as.logical(yahoo_a1[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_ml_lstm$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a2)) {
  ev_soft_yahoo_81_ml_lstm$a2[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_yh_a2[[k]]$detection$event),
             as.logical(yahoo_a2[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_ml_lstm$a2) <- names(yahoo_a2)

#A3
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a3)) {
  ev_soft_yahoo_81_ml_lstm$a3[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_yh_a3[[k]]$detection$event),
             as.logical(yahoo_a3[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_ml_lstm$a3) <- names(yahoo_a3)

#A4
k = 1
for (k in 1:length(result_81_ml_lstm_yh_a4)) {
  ev_soft_yahoo_81_ml_lstm$a4[[k]] <- tryCatch({
    evaluate(har_eval_soft(),
             as.logical(result_81_ml_lstm_yh_a4[[k]]$detection$event),
             as.logical(yahoo_a4[[k]]$event))
  }, error = function(e) {
    message(e)
    return(NA)
  })
}

names(ev_soft_yahoo_81_ml_lstm$a4) <- names(yahoo_a4)

ev_soft_yahoo_81_ml_lstm


## METRICS
#F1_softED
f1_soft_yahoo_81_ml_lstm <- c()

j = 1
for (j in 1:length(ev_soft_yahoo_81_ml_lstm)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(ev_soft_yahoo_81_ml_lstm[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, ev_soft_yahoo_81_ml_lstm[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  yahoo_81_cons$f1_soft <- rbind(yahoo_81_cons$f1_soft, c(method = paste("ML_LSTM_A", j, sep = ""), value = mean(f1_partial, na.rm = TRUE)))
  f1_soft_yahoo_81_ml_lstm <- append(f1_soft_yahoo_81_ml_lstm, f1_partial)
}

names(yahoo_81_cons$f1_soft) <- c("method", "value")

yahoo_81_cons_overall$f1_soft <- rbind(yahoo_81_cons_overall$f1_soft, c(method = "ML_LSTM", value = mean(f1_soft_yahoo_81_ml_lstm, na.rm = TRUE)))


yahoo_81_cons
yahoo_81_cons_overall



##=====
## Save final results
save(yahoo_81_cons, file = "~/janio/harbinger/dev/results/yahoo_81_cons.RData", compress = TRUE)
save(yahoo_81_cons_overall, file = "~/janio/harbinger/dev/results/yahoo_81_cons_overall.RData", compress = TRUE)

#Show complete results
load("~/janio/harbinger/dev/results/yahoo_81_cons.RData")
yahoo_81_cons

load("~/janio/harbinger/dev/results/yahoo_81_cons_overall.RData")
yahoo_81_cons_overall
