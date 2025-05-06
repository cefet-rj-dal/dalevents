#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")


# RARE --------------------------------------------------------------------
load("~/janio/harbinger/dev/data/rare_sample.RData")


## rare_243 -----------------------------------------------------------
##ADJUST BEFORE RUNNING
## rare_243 -----------------------------------------------------------
#Before use loaded data verify if it have the complete experiment results
#If not sure, use individual files instead of complete experiment
#load("exp_rare_243.RData")


#Run the next 4 lines only if decide not to run the last one
load("~/janio/harbinger/dev/results/result_rare_fbiad_243.RData")
load("~/janio/harbinger/dev/results/result_rare_arima_243.RData")
load("~/janio/harbinger/dev/results/result_rare_cf_lr_243.RData")
load("~/janio/harbinger/dev/results/result_rare_garch_243.RData")
load("~/janio/harbinger/dev/results/result_rare_ml_lstm_243.RData")


#Resume ====
prob_cons_rare_243 <- list()
prob_cons_rare_243$acc <- data.frame()
prob_cons_rare_243$f1 <- data.frame()


#Probability limits
plim <- c(0, 0.5, 0.8)


## FBIAD ====
k = 3
prob_ev_rare_243_fbiad <- list()

j = 1
for (j in 1:20) {
  prob <- result_rare_fbiad_243[[j]]$prob
  prob_lim <- subset(prob, pe >= plim[k])
  
  det_prob <- result_rare_fbiad_243[[j]]$detection
  det_prob$event <- 0
  det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
  
  
  prob_ev_rare_243_fbiad[[j]] <- evaluate(result_rare_fbiad_243[[j]]$detector,
                                            det_prob$event,
                                            rare_sample$event) 
}

prob_ev_rare_243_fbiad[[1]]
names(prob_ev_rare_243_fbiad) <- names(rare_sample)[1:20]

#Accuracy
acc_rare_243_fbiad <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_fbiad)) {
  acc_rare_243_fbiad <- tryCatch({
    append(acc_rare_243_fbiad, prob_ev_rare_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_fbiad, NA))
  })
}

acc_rare_243_fbiad


prob_cons_rare_243$acc <- rbind(prob_cons_rare_243$acc, c("fbiad",
                                                          plim[k],
                                                          mean(acc_rare_243_fbiad, na.rm = TRUE)))



names(prob_cons_rare_243$acc) <- c("method", "plim", "value")


#F1
f1_rare_243_fbiad <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_fbiad)) {
  f1_rare_243_fbiad <- tryCatch({
    append(f1_rare_243_fbiad, prob_ev_rare_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_fbiad, NA))
  })
}


f1_rare_243_fbiad


prob_cons_rare_243$f1 <- rbind(prob_cons_rare_243$f1,
                               c(method = "fbiad",
                                    plim = plim[k],
                                    value = mean(f1_rare_243_fbiad, na.rm = TRUE)))

names(prob_cons_rare_243$f1) <- c("method", "plim", "value")


prob_cons_rare_243


#arima
## arima ===============================
k = 3
prob_ev_rare_243_arima <- list()

j = 1


for (j in 1:20) {
  prob <- result_rare_arima_243[[j]]$prob
  prob_lim <- subset(prob, pe >= plim[k])
  
  det_prob <- result_rare_arima_243[[j]]$detection
  det_prob$event <- 0
  det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
  
  
  prob_ev_rare_243_arima[[j]] <- evaluate(result_rare_arima_243[[j]]$detector,
                                          det_prob$event,
                                          rare_sample$event) 
}

prob_ev_rare_243_arima[[1]]
names(prob_ev_rare_243_arima) <- names(rare_sample)[1:20]

#Accuracy
acc_rare_243_arima <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_arima)) {
  acc_rare_243_arima <- tryCatch({
    append(acc_rare_243_arima, prob_ev_rare_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_arima, NA))
  })
}

acc_rare_243_arima


prob_cons_rare_243$acc <- rbind(prob_cons_rare_243$acc, c("arima",
                                                          plim[k],
                                                          mean(acc_rare_243_arima, na.rm = TRUE)))



names(prob_cons_rare_243$acc) <- c("method", "plim", "value")


#F1
f1_rare_243_arima <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_arima)) {
  f1_rare_243_arima <- tryCatch({
    append(f1_rare_243_arima, prob_ev_rare_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_arima, NA))
  })
}


f1_rare_243_arima


prob_cons_rare_243$f1 <- rbind(prob_cons_rare_243$f1,
                               c(method = "arima",
                                 plim = plim[k],
                                 value = mean(f1_rare_243_arima, na.rm = TRUE)))

names(prob_cons_rare_243$f1) <- c("method", "plim", "value")


prob_cons_rare_243


#garch
## garch ===============================
k = 3
prob_ev_rare_243_garch <- list()

j = 1


for (j in 1:20) {
  if (is.list(result_rare_garch_243[[j]])) {
    prob <- result_rare_garch_243[[j]]$prob
    prob_lim <- subset(prob, pe >= plim[k])
    
    det_prob <- result_rare_garch_243[[j]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    prob_ev_rare_243_garch[[j]] <- tryCatch({
      evaluate(result_rare_garch_243[[j]]$detector,
               det_prob$event,
               rare_sample$event)
    }, error = function(e){
      message(e)
      return(NA)
    })
  } else {
    prob_ev_rare_243_garch[[j]] <- NA
  }
    
}

prob_ev_rare_243_garch[[1]]
names(prob_ev_rare_243_garch) <- names(rare_sample)[1:20]

#Accuracy
acc_rare_243_garch <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_garch)) {
  acc_rare_243_garch <- tryCatch({
    append(acc_rare_243_garch, prob_ev_rare_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_garch, NA))
  })
}

acc_rare_243_garch


prob_cons_rare_243$acc <- rbind(prob_cons_rare_243$acc, c("garch",
                                                          plim[k],
                                                          mean(acc_rare_243_garch, na.rm = TRUE)))



names(prob_cons_rare_243$acc) <- c("method", "plim", "value")


#F1
f1_rare_243_garch <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_garch)) {
  f1_rare_243_garch <- tryCatch({
    append(f1_rare_243_garch, prob_ev_rare_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_garch, NA))
  })
}


f1_rare_243_garch


prob_cons_rare_243$f1 <- rbind(prob_cons_rare_243$f1,
                               c(method = "garch",
                                 plim = plim[k],
                                 value = mean(f1_rare_243_garch, na.rm = TRUE)))

names(prob_cons_rare_243$f1) <- c("method", "plim", "value")


prob_cons_rare_243


#cf_lr
## cf_lr ===============================
k = 3
prob_ev_rare_243_cf_lr <- list()

j = 1


for (j in 1:20) {
  if (is.list(result_rare_cf_lr_243[[j]])) {
    prob <- result_rare_cf_lr_243[[j]]$prob
    prob_lim <- subset(prob, pe >= plim[k])
    
    det_prob <- result_rare_cf_lr_243[[j]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    prob_ev_rare_243_cf_lr[[j]] <- tryCatch({
      evaluate(result_rare_cf_lr_243[[j]]$detector,
               det_prob$event,
               rare_sample$event)
    }, error = function(e){
      message(e)
      return(NA)
    })
  } else {
    prob_ev_rare_243_cf_lr[[j]] <- NA
  }
  
}

prob_ev_rare_243_cf_lr[[1]]
names(prob_ev_rare_243_cf_lr) <- names(rare_sample)[1:20]

#Accuracy
acc_rare_243_cf_lr <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_cf_lr)) {
  acc_rare_243_cf_lr <- tryCatch({
    append(acc_rare_243_cf_lr, prob_ev_rare_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_cf_lr, NA))
  })
}

acc_rare_243_cf_lr


prob_cons_rare_243$acc <- rbind(prob_cons_rare_243$acc, c("cf_lr",
                                                          plim[k],
                                                          mean(acc_rare_243_cf_lr, na.rm = TRUE)))



names(prob_cons_rare_243$acc) <- c("method", "plim", "value")


#F1
f1_rare_243_cf_lr <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_cf_lr)) {
  f1_rare_243_cf_lr <- tryCatch({
    append(f1_rare_243_cf_lr, prob_ev_rare_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_cf_lr, NA))
  })
}


f1_rare_243_cf_lr


prob_cons_rare_243$f1 <- rbind(prob_cons_rare_243$f1,
                               c(method = "cf_lr",
                                 plim = plim[k],
                                 value = mean(f1_rare_243_cf_lr, na.rm = TRUE)))

names(prob_cons_rare_243$f1) <- c("method", "plim", "value")


prob_cons_rare_243



#ml_lstm
## ml lstm ===============================
k = 3
prob_ev_rare_243_ml_lstm <- list()

j = 1


for (j in 1:20) {
  if (is.list(result_rare_ml_lstm_243[[j]])) {
    prob <- result_rare_ml_lstm_243[[j]]$prob
    prob_lim <- subset(prob, pe >= plim[k])
    
    det_prob <- result_rare_ml_lstm_243[[j]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    prob_ev_rare_243_ml_lstm[[j]] <- tryCatch({
      evaluate(result_rare_ml_lstm_243[[j]]$detector,
               det_prob$event,
               rare_sample$event)
    }, error = function(e){
      message(e)
      return(NA)
    })
  } else {
    prob_ev_rare_243_ml_lstm[[j]] <- NA
  }
  
}

prob_ev_rare_243_ml_lstm[[1]]
names(prob_ev_rare_243_ml_lstm) <- names(rare_sample)[1:20]

#Accuracy
acc_rare_243_ml_lstm <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_ml_lstm)) {
  acc_rare_243_ml_lstm <- tryCatch({
    append(acc_rare_243_ml_lstm, prob_ev_rare_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_rare_243_ml_lstm, NA))
  })
}

acc_rare_243_ml_lstm


prob_cons_rare_243$acc <- rbind(prob_cons_rare_243$acc, c("ml_lstm",
                                                          plim[k],
                                                          mean(acc_rare_243_ml_lstm, na.rm = TRUE)))



names(prob_cons_rare_243$acc) <- c("method", "plim", "value")


#F1
f1_rare_243_ml_lstm <- c()

j = 1

for (j in 1:length(prob_ev_rare_243_ml_lstm)) {
  f1_rare_243_ml_lstm <- tryCatch({
    append(f1_rare_243_ml_lstm, prob_ev_rare_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_rare_243_ml_lstm, NA))
  })
}


f1_rare_243_ml_lstm


prob_cons_rare_243$f1 <- rbind(prob_cons_rare_243$f1,
                               c(method = "ml_lstm",
                                 plim = plim[k],
                                 value = mean(f1_rare_243_ml_lstm, na.rm = TRUE)))

names(prob_cons_rare_243$f1) <- c("method", "plim", "value")


prob_cons_rare_243


##=====
## Save final results
save(prob_cons_rare_243, file = "~/janio/nexus/consolidation/prob/prob_cons_rare_243.RData", compress = TRUE)

#Show complete results
#load("~/janio/harbinger/dev/results/prob_cons_rare_243.RData")
prob_cons_rare_243

