#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/CEFET/nexus/env_start.R")


# YAHOO --------------------------------------------------------------------
data(yahoo_a1)
data(yahoo_a2)
data(yahoo_a3)
data(yahoo_a4)

## yahoo_243 -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
#FBIAD RESULTS
load("~/janio/harbinger/dev/results/result_243_fbiad_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_243_fbiad_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_243_fbiad_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_243_fbiad_yh_a4_complete.RData")

#ARIMA RESULTS
load("~/janio/harbinger/dev/results/result_243_arima_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_243_arima_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_243_arima_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_243_arima_yh_a4_complete.RData")

#GARCH RESULTS
load("~/janio/harbinger/dev/results/result_243_garch_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_243_garch_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_243_garch_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_243_garch_yh_a4_complete.RData")

#CF_LR RESULTS
load("~/janio/harbinger/dev/results/result_243_cf_lr_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_243_cf_lr_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_243_cf_lr_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_243_cf_lr_yh_a4_complete.RData")


#ML_LSTM RESULTS
load("~/janio/harbinger/dev/results/result_243_ml_lstm_yh_a1_complete.RData")
load("~/janio/harbinger/dev/results/result_243_ml_lstm_yh_a2_complete.RData")
load("~/janio/harbinger/dev/results/result_243_ml_lstm_yh_a3_complete.RData")
load("~/janio/harbinger/dev/results/result_243_ml_lstm_yh_a4_complete.RData")


#Resume
prob_yahoo_243_cons <- list()
prob_yahoo_243_cons_overall <- list()
prob_yahoo_243_cons$acc <- data.frame()
prob_yahoo_243_cons_overall$acc <- data.frame()
prob_yahoo_243_cons$f1 <- data.frame()
prob_yahoo_243_cons_overall$f1 <- data.frame()



#Probability limits
plim <- c(0, 0.5, 0.8)


#FBIAD =========
i = 3

prob_ev_yahoo_243_fbiad <- list()

#A1
k = 1
for (k in 1:length(result_243_fbiad_yh_a1)) {
  if (is.list(result_243_fbiad_yh_a1[[k]])){
    prob <- result_243_fbiad_yh_a1[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_yh_a1[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_fbiad$a1[[k]] <- tryCatch({
      evaluate(result_243_fbiad_yh_a1[[k]]$detector,
               det_prob$event,
               yahoo_a1[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_fbiad$a1[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_fbiad$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_243_fbiad_yh_a2)) {
  if (is.list(result_243_fbiad_yh_a2[[k]])){
    prob <- result_243_fbiad_yh_a2[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_yh_a2[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_fbiad$a2[[k]] <- tryCatch({
      evaluate(result_243_fbiad_yh_a2[[k]]$detector,
               det_prob$event,
               yahoo_a2[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_fbiad$a2[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_fbiad$a2) <- names(yahoo_a2)


#A3
k = 1
for (k in 1:length(result_243_fbiad_yh_a3)) {
  if (is.list(result_243_fbiad_yh_a3[[k]])){
    prob <- result_243_fbiad_yh_a3[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_yh_a3[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_fbiad$a3[[k]] <- tryCatch({
      evaluate(result_243_fbiad_yh_a3[[k]]$detector,
               det_prob$event,
               yahoo_a3[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_fbiad$a3[[k]] <- NA
  }
}

#A4
k = 1
for (k in 1:length(result_243_fbiad_yh_a4)) {
  if (is.list(result_243_fbiad_yh_a4[[k]])){
    prob <- result_243_fbiad_yh_a4[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_yh_a4[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_fbiad$a4[[k]] <- tryCatch({
      evaluate(result_243_fbiad_yh_a4[[k]]$detector,
               det_prob$event,
               yahoo_a4[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_fbiad$a4[[k]] <- NA
  }
}
names(prob_ev_yahoo_243_fbiad$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_243_fbiad <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_fbiad)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_fbiad[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, prob_ev_yahoo_243_fbiad[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  prob_yahoo_243_cons$acc <- rbind(prob_yahoo_243_cons$acc, c(method = paste("FBIAD_A", j, sep = ""), plim=plim[i], value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_243_fbiad <- append(acc_yahoo_243_fbiad, acc_partial)
}
names(prob_yahoo_243_cons$acc) <- c("method","plim","value")

prob_yahoo_243_cons_overall$acc <- rbind(prob_yahoo_243_cons_overall$acc, c(method = "FBIAD", plim=plim[i], value = mean(acc_yahoo_243_fbiad, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$acc) <- c("method","plim","value")

prob_yahoo_243_cons
prob_yahoo_243_cons_overall


#F1
f1_yahoo_243_fbiad <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_fbiad)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_fbiad[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, prob_ev_yahoo_243_fbiad[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  prob_yahoo_243_cons$f1 <- rbind(prob_yahoo_243_cons$f1, c(method = paste("FBIAD_A", j, sep = ""), plim=plim[i], value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_243_fbiad <- append(f1_yahoo_243_fbiad, f1_partial)
}

names(prob_yahoo_243_cons$f1) <- c("method", "plim","value")

prob_yahoo_243_cons_overall$f1 <- rbind(prob_yahoo_243_cons_overall$f1, c(method = "FBIAD", plim=plim[i], value = mean(f1_yahoo_243_fbiad, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$f1) <- c("method", "plim","value")

prob_yahoo_243_cons
prob_yahoo_243_cons_overall


#arima =================
i = 3

prob_ev_yahoo_243_arima <- list()

#A1
k = 1
for (k in 1:length(result_243_arima_yh_a1)) {
  if (is.list(result_243_arima_yh_a1[[k]])){
    prob <- result_243_arima_yh_a1[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_yh_a1[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_arima$a1[[k]] <- tryCatch({
      evaluate(result_243_arima_yh_a1[[k]]$detector,
               det_prob$event,
               yahoo_a1[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_arima$a1[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_arima$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_243_arima_yh_a2)) {
  if (is.list(result_243_arima_yh_a2[[k]])){
    prob <- result_243_arima_yh_a2[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_yh_a2[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_arima$a2[[k]] <- tryCatch({
      evaluate(result_243_arima_yh_a2[[k]]$detector,
               det_prob$event,
               yahoo_a2[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_arima$a2[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_arima$a2) <- names(yahoo_a2)


#A3
k = 1
for (k in 1:length(result_243_arima_yh_a3)) {
  if (is.list(result_243_arima_yh_a3[[k]])){
    prob <- result_243_arima_yh_a3[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_yh_a3[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_arima$a3[[k]] <- tryCatch({
      evaluate(result_243_arima_yh_a3[[k]]$detector,
               det_prob$event,
               yahoo_a3[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_arima$a3[[k]] <- NA
  }
}

#A4
k = 1
for (k in 1:length(result_243_arima_yh_a4)) {
  if (is.list(result_243_arima_yh_a4[[k]])){
    prob <- result_243_arima_yh_a4[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_yh_a4[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_arima$a4[[k]] <- tryCatch({
      evaluate(result_243_arima_yh_a4[[k]]$detector,
               det_prob$event,
               yahoo_a4[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_arima$a4[[k]] <- NA
  }
}
names(prob_ev_yahoo_243_arima$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_243_arima <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_arima)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_arima[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, prob_ev_yahoo_243_arima[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  prob_yahoo_243_cons$acc <- rbind(prob_yahoo_243_cons$acc, c(method = paste("arima_A", j, sep = ""), plim=plim[i], value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_243_arima <- append(acc_yahoo_243_arima, acc_partial)
}
names(prob_yahoo_243_cons$acc) <- c("method","plim","value")

prob_yahoo_243_cons_overall$acc <- rbind(prob_yahoo_243_cons_overall$acc, c(method = "arima", plim=plim[i], value = mean(acc_yahoo_243_arima, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$acc) <- c("method","plim","value")

prob_yahoo_243_cons
prob_yahoo_243_cons_overall


#F1
f1_yahoo_243_arima <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_arima)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_arima[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, prob_ev_yahoo_243_arima[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  prob_yahoo_243_cons$f1 <- rbind(prob_yahoo_243_cons$f1, c(method = paste("arima_A", j, sep = ""), plim=plim[i], value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_243_arima <- append(f1_yahoo_243_arima, f1_partial)
}

names(prob_yahoo_243_cons$f1) <- c("method", "plim","value")

prob_yahoo_243_cons_overall$f1 <- rbind(prob_yahoo_243_cons_overall$f1, c(method = "arima", plim=plim[i], value = mean(f1_yahoo_243_arima, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$f1) <- c("method", "plim","value")

prob_yahoo_243_cons
prob_yahoo_243_cons_overall


## GARCH ##============================
i = 3


prob_ev_yahoo_243_garch <- list()

#A1
k = 1
for (k in 1:length(result_243_garch_yh_a1)) {
  if (is.list(result_243_garch_yh_a1[[k]])){
    prob <- result_243_garch_yh_a1[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_yh_a1[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_garch$a1[[k]] <- tryCatch({
      evaluate(result_243_garch_yh_a1[[k]]$detector,
               det_prob$event,
               yahoo_a1[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_garch$a1[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_garch$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_243_garch_yh_a2)) {
  if (is.list(result_243_garch_yh_a2[[k]])){
    prob <- result_243_garch_yh_a2[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_yh_a2[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_garch$a2[[k]] <- tryCatch({
      evaluate(result_243_garch_yh_a2[[k]]$detector,
               det_prob$event,
               yahoo_a2[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_garch$a2[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_garch$a2) <- names(yahoo_a2)


#A3
k = 1
for (k in 1:length(result_243_garch_yh_a3)) {
  if (is.list(result_243_garch_yh_a3[[k]])){
    prob <- result_243_garch_yh_a3[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_yh_a3[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_garch$a3[[k]] <- tryCatch({
      evaluate(result_243_garch_yh_a3[[k]]$detector,
               det_prob$event,
               yahoo_a3[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_garch$a3[[k]] <- NA
  }
}

#A4
k = 1
for (k in 1:length(result_243_garch_yh_a4)) {
  if (is.list(result_243_garch_yh_a4[[k]])){
    prob <- result_243_garch_yh_a4[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_yh_a4[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_garch$a4[[k]] <- tryCatch({
      evaluate(result_243_garch_yh_a4[[k]]$detector,
               det_prob$event,
               yahoo_a4[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_garch$a4[[k]] <- NA
  }
}
names(prob_ev_yahoo_243_garch$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_243_garch <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_garch)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_garch[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, prob_ev_yahoo_243_garch[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  prob_yahoo_243_cons$acc <- rbind(prob_yahoo_243_cons$acc, c(method = paste("garch_A", j, sep = ""), plim=plim[i], value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_243_garch <- append(acc_yahoo_243_garch, acc_partial)
}
names(prob_yahoo_243_cons$acc) <- c("method","plim","value")

prob_yahoo_243_cons_overall$acc <- rbind(prob_yahoo_243_cons_overall$acc, c(method = "garch", plim=plim[i], value = mean(acc_yahoo_243_garch, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$acc) <- c("method","plim","value")

prob_yahoo_243_cons
prob_yahoo_243_cons_overall


#F1
f1_yahoo_243_garch <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_garch)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_garch[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, prob_ev_yahoo_243_garch[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  prob_yahoo_243_cons$f1 <- rbind(prob_yahoo_243_cons$f1, c(method = paste("garch_A", j, sep = ""), plim=plim[i], value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_243_garch <- append(f1_yahoo_243_garch, f1_partial)
}

names(prob_yahoo_243_cons$f1) <- c("method", "plim","value")

prob_yahoo_243_cons_overall$f1 <- rbind(prob_yahoo_243_cons_overall$f1, c(method = "garch", plim=plim[i], value = mean(f1_yahoo_243_garch, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$f1) <- c("method", "plim","value")


## CF ##=======================================
i = 3


prob_ev_yahoo_243_cf_lr <- list()

#A1
k = 1
for (k in 1:length(result_243_cf_lr_yh_a1)) {
  if (is.list(result_243_cf_lr_yh_a1[[k]])){
    prob <- result_243_cf_lr_yh_a1[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_yh_a1[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_cf_lr$a1[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_yh_a1[[k]]$detector,
               det_prob$event,
               yahoo_a1[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_cf_lr$a1[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_cf_lr$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_243_cf_lr_yh_a2)) {
  if (is.list(result_243_cf_lr_yh_a2[[k]])){
    prob <- result_243_cf_lr_yh_a2[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_yh_a2[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_cf_lr$a2[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_yh_a2[[k]]$detector,
               det_prob$event,
               yahoo_a2[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_cf_lr$a2[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_cf_lr$a2) <- names(yahoo_a2)


#A3
k = 1
for (k in 1:length(result_243_cf_lr_yh_a3)) {
  if (is.list(result_243_cf_lr_yh_a3[[k]])){
    prob <- result_243_cf_lr_yh_a3[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_yh_a3[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_cf_lr$a3[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_yh_a3[[k]]$detector,
               det_prob$event,
               yahoo_a3[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_cf_lr$a3[[k]] <- NA
  }
}

#A4
k = 1
for (k in 1:length(result_243_cf_lr_yh_a4)) {
  if (is.list(result_243_cf_lr_yh_a4[[k]])){
    prob <- result_243_cf_lr_yh_a4[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_yh_a4[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_cf_lr$a4[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_yh_a4[[k]]$detector,
               det_prob$event,
               yahoo_a4[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_cf_lr$a4[[k]] <- NA
  }
}
names(prob_ev_yahoo_243_cf_lr$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_243_cf_lr <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_cf_lr)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_cf_lr[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, prob_ev_yahoo_243_cf_lr[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  prob_yahoo_243_cons$acc <- rbind(prob_yahoo_243_cons$acc, c(method = paste("cf_lr_A", j, sep = ""), plim=plim[i], value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_243_cf_lr <- append(acc_yahoo_243_cf_lr, acc_partial)
}
names(prob_yahoo_243_cons$acc) <- c("method","plim","value")

prob_yahoo_243_cons_overall$acc <- rbind(prob_yahoo_243_cons_overall$acc, c(method = "cf_lr", plim=plim[i], value = mean(acc_yahoo_243_cf_lr, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$acc) <- c("method","plim","value")


#F1
f1_yahoo_243_cf_lr <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_cf_lr)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_cf_lr[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, prob_ev_yahoo_243_cf_lr[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  prob_yahoo_243_cons$f1 <- rbind(prob_yahoo_243_cons$f1, c(method = paste("cf_lr_A", j, sep = ""), plim=plim[i], value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_243_cf_lr <- append(f1_yahoo_243_cf_lr, f1_partial)
}

names(prob_yahoo_243_cons$f1) <- c("method", "plim","value")

prob_yahoo_243_cons_overall$f1 <- rbind(prob_yahoo_243_cons_overall$f1, c(method = "cf_lr", plim=plim[i], value = mean(f1_yahoo_243_cf_lr, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$f1) <- c("method", "plim","value")



## LSTM ##=====================
i = 3


prob_ev_yahoo_243_ml_lstm <- list()

#A1
k = 1
for (k in 1:length(result_243_ml_lstm_yh_a1)) {
  if (is.list(result_243_ml_lstm_yh_a1[[k]])){
    prob <- result_243_ml_lstm_yh_a1[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_yh_a1[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_ml_lstm$a1[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_yh_a1[[k]]$detector,
               det_prob$event,
               yahoo_a1[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_ml_lstm$a1[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_ml_lstm$a1) <- names(yahoo_a1)

#A2
k = 1
for (k in 1:length(result_243_ml_lstm_yh_a2)) {
  if (is.list(result_243_ml_lstm_yh_a2[[k]])){
    prob <- result_243_ml_lstm_yh_a2[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_yh_a2[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_ml_lstm$a2[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_yh_a2[[k]]$detector,
               det_prob$event,
               yahoo_a2[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_ml_lstm$a2[[k]] <- NA
  }
}

names(prob_ev_yahoo_243_ml_lstm$a2) <- names(yahoo_a2)


#A3
k = 1
for (k in 1:length(result_243_ml_lstm_yh_a3)) {
  if (is.list(result_243_ml_lstm_yh_a3[[k]])){
    prob <- result_243_ml_lstm_yh_a3[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_yh_a3[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_ml_lstm$a3[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_yh_a3[[k]]$detector,
               det_prob$event,
               yahoo_a3[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_ml_lstm$a3[[k]] <- NA
  }
}

#A4
k = 1
for (k in 1:length(result_243_ml_lstm_yh_a4)) {
  if (is.list(result_243_ml_lstm_yh_a4[[k]])){
    prob <- result_243_ml_lstm_yh_a4[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_yh_a4[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    prob_ev_yahoo_243_ml_lstm$a4[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_yh_a4[[k]]$detector,
               det_prob$event,
               yahoo_a4[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    prob_ev_yahoo_243_ml_lstm$a4[[k]] <- NA
  }
}
names(prob_ev_yahoo_243_ml_lstm$a4) <- names(yahoo_a4)


## METRICS
#Accuracy
acc_yahoo_243_ml_lstm <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_ml_lstm)) {
  acc_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_ml_lstm[[j]])) {
    acc_partial <- tryCatch({
      append(acc_partial, prob_ev_yahoo_243_ml_lstm[[j]][[k]]$accuracy)
    }, error = function(e) {
      message(e)
      return(append(acc_partial, NA))
    })
  }
  prob_yahoo_243_cons$acc <- rbind(prob_yahoo_243_cons$acc, c(method = paste("ml_lstm_A", j, sep = ""), plim=plim[i], value = mean(acc_partial, na.rm = TRUE)))
  acc_yahoo_243_ml_lstm <- append(acc_yahoo_243_ml_lstm, acc_partial)
}
names(prob_yahoo_243_cons$acc) <- c("method","plim","value")

prob_yahoo_243_cons_overall$acc <- rbind(prob_yahoo_243_cons_overall$acc, c(method = "ml_lstm", plim=plim[i], value = mean(acc_yahoo_243_ml_lstm, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$acc) <- c("method","plim","value")



#F1
f1_yahoo_243_ml_lstm <- c()

j = 1


for (j in 1:length(prob_ev_yahoo_243_ml_lstm)) {
  f1_partial <- c()
  k = 1
  for (k in 1:length(prob_ev_yahoo_243_ml_lstm[[j]])) {
    f1_partial <- tryCatch({
      append(f1_partial, prob_ev_yahoo_243_ml_lstm[[j]][[k]]$F1)
    }, error = function(e) {
      message(e)
      return(append(f1_partial, NA))
    })
  }
  prob_yahoo_243_cons$f1 <- rbind(prob_yahoo_243_cons$f1, c(method = paste("ml_lstm_A", j, sep = ""), plim=plim[i], value = mean(f1_partial, na.rm = TRUE)))
  f1_yahoo_243_ml_lstm <- append(f1_yahoo_243_ml_lstm, f1_partial)
}

names(prob_yahoo_243_cons$f1) <- c("method", "plim","value")

prob_yahoo_243_cons_overall$f1 <- rbind(prob_yahoo_243_cons_overall$f1, c(method = "ml_lstm", plim=plim[i], value = mean(f1_yahoo_243_ml_lstm, na.rm = TRUE)))
names(prob_yahoo_243_cons_overall$f1) <- c("method", "plim","value")



##=====
prob_yahoo_243_cons
prob_yahoo_243_cons_overall

## Save final results
save(prob_yahoo_243_cons, file ="~/CEFET/nexus/consolidation/prob/prob_yahoo_243_cons.RData", compress = TRUE)
save(prob_yahoo_243_cons_overall, file ="~/CEFET/nexus/consolidation/prob/prob_yahoo_243_cons_overall.RData", compress = TRUE)

#Show complete results
#load("~/janio/harbinger/dev/results/prob_yahoo_243_cons.RData")
prob_yahoo_243_cons

#load("~/janio/harbinger/dev/results/prob_yahoo_243_cons_overall.RData")
prob_yahoo_243_cons_overall

