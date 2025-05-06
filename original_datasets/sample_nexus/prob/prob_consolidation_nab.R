#Results consolidations
#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/CEFET/nexus/env_start.R")
load("~/CEFET/nexus/consolidation/prob/workspace_prob_cons_nab.RData")
setwd("~/CEFET/nexus/consolidation/prob")


#Resume
prob_numenta_243_cons <- list()
prob_numenta_243_cons_overall <- list()
prob_numenta_243_cons$acc <- data.frame()
prob_numenta_243_cons_overall$acc <- data.frame()
prob_numenta_243_cons$f1 <- data.frame()
prob_numenta_243_cons_overall$f1 <- data.frame()


#Probability limits
plim <- c(0, 0.5, 0.8)
i = 3

#FBIAD =========
## art_anomaly -----------------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_art_anom)) {
  if(is.list(result_243_fbiad_numenta_art_anom[[k]])){
    prob <- result_243_fbiad_numenta_art_anom[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_art_anom[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_art_anom[[k]]$detector,
               det_prob$event,
               numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_fbiad <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                              c(method = "FBIAD_ArtAnom",
                                plim = plim[i],
                                value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_numenta_243_fbiad <- c() #All series to create overall results

f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                             c(method = "FBIAD_ArtAnom",
                               plim = plim[i],
                               value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1

## REAL AD EXC -----------------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_real_adexc)) {
  if(is.list(result_243_fbiad_numenta_real_adexc[[k]])){
    prob <- result_243_fbiad_numenta_real_adexc[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_real_adexc[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_real_adexc[[k]]$detector,
               det_prob$event,
               numenta_realAdExchange$realAdExchange[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "FBIAD_ReadAdExch",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "FBIAD_ReadAdExch",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1



## REAL AWS -----------------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_real_aws)) {
  if(is.list(result_243_fbiad_numenta_real_aws[[k]])){
    prob <- result_243_fbiad_numenta_real_aws[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_real_aws[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_real_aws[[k]]$detector,
               det_prob$event,
               numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "FBIAD_RealAWS",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "FBIAD_RealAWS",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Known Cause ----------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_real_knwcs)) {
  if(is.list(result_243_fbiad_numenta_real_knwcs[[k]])){
    prob <- result_243_fbiad_numenta_real_knwcs[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_real_knwcs[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_real_knwcs[[k]]$detector,
               det_prob$event,
               numenta_realKnownCause$realKnownCause[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "FBIAD_RealKnownCause",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "FBIAD_RealKnownCause",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Traffic ----------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_real_traff)) {
  if(is.list(result_243_fbiad_numenta_real_traff[[k]])){
    prob <- result_243_fbiad_numenta_real_traff[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_real_traff[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_real_traff[[k]]$detector,
               det_prob$event,
               numenta_realTraffic$realTraffic[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "FBIAD_RealTraffic",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "FBIAD_RealTraffic",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Tweets ----------------------------------------------------
ev_numenta_243_fbiad <- list()


k = 1
for (k in 1:length(result_243_fbiad_numenta_real_tweets)) {
  if(is.list(result_243_fbiad_numenta_real_tweets[[k]])){
    prob <- result_243_fbiad_numenta_real_tweets[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_fbiad_numenta_real_tweets[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_fbiad[[k]] <- tryCatch({
      evaluate(result_243_fbiad_numenta_real_tweets[[k]]$detector,
               det_prob$event,
               numenta_realTweets$realTweets[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_fbiad[[k]] <- NA
  }
}

names(ev_numenta_243_fbiad) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_fbiad[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_fbiad <- append(acc_numenta_243_fbiad, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "FBIAD_RealTweets",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_fbiad)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_fbiad[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_fbiad <- append(f1_numenta_243_fbiad, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "FBIAD_RealTweets",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


# #Method consolidation ---------------------------------------------------
acc_numenta_243_fbiad
f1_numenta_243_fbiad

prob_numenta_243_cons_overall$acc <- rbind(prob_numenta_243_cons_overall$acc,
                                           c("FBIAD",
                                             plim[i],
                                             mean(acc_numenta_243_fbiad, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$acc) <- c("method", "plim", "value")

prob_numenta_243_cons_overall$f1 <- rbind(prob_numenta_243_cons_overall$f1,
                                           c("FBIAD",
                                             plim[i],
                                             mean(f1_numenta_243_fbiad, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$f1) <- c("method", "plim", "value")

#ARIMA =========
## art_anomaly -----------------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_art_anom)) {
  if(is.list(result_243_arima_numenta_art_anom[[k]])){
    prob <- result_243_arima_numenta_art_anom[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_art_anom[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_art_anom[[k]]$detector,
               det_prob$event,
               numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_arima <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_ArtAnom",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_numenta_243_arima <- c() #All series to create overall results

f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_ArtAnom",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1

## REAL AD EXC -----------------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_real_adexc)) {
  if(is.list(result_243_arima_numenta_real_adexc[[k]])){
    prob <- result_243_arima_numenta_real_adexc[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_real_adexc[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_real_adexc[[k]]$detector,
               det_prob$event,
               numenta_realAdExchange$realAdExchange[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_ReadAdExch",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_ReadAdExch",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1



## REAL AWS -----------------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_real_aws)) {
  if(is.list(result_243_arima_numenta_real_aws[[k]])){
    prob <- result_243_arima_numenta_real_aws[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_real_aws[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_real_aws[[k]]$detector,
               det_prob$event,
               numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_RealAWS",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_RealAWS",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Known Cause ----------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_real_knwcs)) {
  if(is.list(result_243_arima_numenta_real_knwcs[[k]])){
    prob <- result_243_arima_numenta_real_knwcs[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_real_knwcs[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_real_knwcs[[k]]$detector,
               det_prob$event,
               numenta_realKnownCause$realKnownCause[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_RealKnownCause",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_RealKnownCause",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Traffic ----------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_real_traff)) {
  if(is.list(result_243_arima_numenta_real_traff[[k]])){
    prob <- result_243_arima_numenta_real_traff[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_real_traff[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_real_traff[[k]]$detector,
               det_prob$event,
               numenta_realTraffic$realTraffic[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_RealTraffic",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_RealTraffic",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Tweets ----------------------------------------------------
ev_numenta_243_arima <- list()


k = 1
for (k in 1:length(result_243_arima_numenta_real_tweets)) {
  if(is.list(result_243_arima_numenta_real_tweets[[k]])){
    prob <- result_243_arima_numenta_real_tweets[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_arima_numenta_real_tweets[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_arima[[k]] <- tryCatch({
      evaluate(result_243_arima_numenta_real_tweets[[k]]$detector,
               det_prob$event,
               numenta_realTweets$realTweets[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_arima[[k]] <- NA
  }
}

names(ev_numenta_243_arima) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_arima[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_arima <- append(acc_numenta_243_arima, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "arima_RealTweets",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_arima)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_arima[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_arima <- append(f1_numenta_243_arima, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "arima_RealTweets",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


# #Method consolidation ---------------------------------------------------
acc_numenta_243_arima
f1_numenta_243_arima

prob_numenta_243_cons_overall$acc <- rbind(prob_numenta_243_cons_overall$acc,
                                           c("arima",
                                             plim[i],
                                             mean(acc_numenta_243_arima, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$acc) <- c("method", "plim", "value")

prob_numenta_243_cons_overall$f1 <- rbind(prob_numenta_243_cons_overall$f1,
                                          c("arima",
                                            plim[i],
                                            mean(f1_numenta_243_arima, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$f1) <- c("method", "plim", "value")

#GARCH =========
## art_anomaly -----------------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_art_anom)) {
  if(is.list(result_243_garch_numenta_art_anom[[k]])){
    prob <- result_243_garch_numenta_art_anom[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_art_anom[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_art_anom[[k]]$detector,
               det_prob$event,
               numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_garch <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_ArtAnom",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_numenta_243_garch <- c() #All series to create overall results

f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_ArtAnom",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1

## REAL AD EXC -----------------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_real_adexc)) {
  if(is.list(result_243_garch_numenta_real_adexc[[k]])){
    prob <- result_243_garch_numenta_real_adexc[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_real_adexc[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_real_adexc[[k]]$detector,
               det_prob$event,
               numenta_realAdExchange$realAdExchange[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_ReadAdExch",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_ReadAdExch",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1



## REAL AWS -----------------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_real_aws)) {
  if(is.list(result_243_garch_numenta_real_aws[[k]])){
    prob <- result_243_garch_numenta_real_aws[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_real_aws[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_real_aws[[k]]$detector,
               det_prob$event,
               numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_RealAWS",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_RealAWS",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Known Cause ----------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_real_knwcs)) {
  if(is.list(result_243_garch_numenta_real_knwcs[[k]])){
    prob <- result_243_garch_numenta_real_knwcs[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_real_knwcs[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_real_knwcs[[k]]$detector,
               det_prob$event,
               numenta_realKnownCause$realKnownCause[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_RealKnownCause",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_RealKnownCause",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Traffic ----------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_real_traff)) {
  if(is.list(result_243_garch_numenta_real_traff[[k]])){
    prob <- result_243_garch_numenta_real_traff[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_real_traff[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_real_traff[[k]]$detector,
               det_prob$event,
               numenta_realTraffic$realTraffic[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_RealTraffic",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_RealTraffic",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Tweets ----------------------------------------------------
ev_numenta_243_garch <- list()


k = 1
for (k in 1:length(result_243_garch_numenta_real_tweets)) {
  if(is.list(result_243_garch_numenta_real_tweets[[k]])){
    prob <- result_243_garch_numenta_real_tweets[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_garch_numenta_real_tweets[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_garch[[k]] <- tryCatch({
      evaluate(result_243_garch_numenta_real_tweets[[k]]$detector,
               det_prob$event,
               numenta_realTweets$realTweets[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_garch[[k]] <- NA
  }
}

names(ev_numenta_243_garch) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_garch[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_garch <- append(acc_numenta_243_garch, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "garch_RealTweets",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_garch)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_garch[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_garch <- append(f1_numenta_243_garch, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "garch_RealTweets",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


# #Method consolidation ---------------------------------------------------
acc_numenta_243_garch
f1_numenta_243_garch

prob_numenta_243_cons_overall$acc <- rbind(prob_numenta_243_cons_overall$acc,
                                           c("garch",
                                             plim[i],
                                             mean(acc_numenta_243_garch, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$acc) <- c("method", "plim", "value")

prob_numenta_243_cons_overall$f1 <- rbind(prob_numenta_243_cons_overall$f1,
                                          c("garch",
                                            plim[i],
                                            mean(f1_numenta_243_garch, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$f1) <- c("method", "plim", "value")

#CF =========
## art_anomaly -----------------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_art_anom)) {
  if(is.list(result_243_cf_lr_numenta_art_anom[[k]])){
    prob <- result_243_cf_lr_numenta_art_anom[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_art_anom[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_art_anom[[k]]$detector,
               det_prob$event,
               numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_cf_lr <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_ArtAnom",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_numenta_243_cf_lr <- c() #All series to create overall results

f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_ArtAnom",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1

## REAL AD EXC -----------------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_real_adexc)) {
  if(is.list(result_243_cf_lr_numenta_real_adexc[[k]])){
    prob <- result_243_cf_lr_numenta_real_adexc[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_real_adexc[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_real_adexc[[k]]$detector,
               det_prob$event,
               numenta_realAdExchange$realAdExchange[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_ReadAdExch",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_ReadAdExch",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1



## REAL AWS -----------------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_real_aws)) {
  if(is.list(result_243_cf_lr_numenta_real_aws[[k]])){
    prob <- result_243_cf_lr_numenta_real_aws[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_real_aws[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_real_aws[[k]]$detector,
               det_prob$event,
               numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_RealAWS",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_RealAWS",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Known Cause ----------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_real_knwcs)) {
  if(is.list(result_243_cf_lr_numenta_real_knwcs[[k]])){
    prob <- result_243_cf_lr_numenta_real_knwcs[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_real_knwcs[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_real_knwcs[[k]]$detector,
               det_prob$event,
               numenta_realKnownCause$realKnownCause[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_RealKnownCause",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_RealKnownCause",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Traffic ----------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_real_traff)) {
  if(is.list(result_243_cf_lr_numenta_real_traff[[k]])){
    prob <- result_243_cf_lr_numenta_real_traff[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_real_traff[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_real_traff[[k]]$detector,
               det_prob$event,
               numenta_realTraffic$realTraffic[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_RealTraffic",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_RealTraffic",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Tweets ----------------------------------------------------
ev_numenta_243_cf_lr <- list()


k = 1
for (k in 1:length(result_243_cf_lr_numenta_real_tweets)) {
  if(is.list(result_243_cf_lr_numenta_real_tweets[[k]])){
    prob <- result_243_cf_lr_numenta_real_tweets[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_cf_lr_numenta_real_tweets[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_cf_lr[[k]] <- tryCatch({
      evaluate(result_243_cf_lr_numenta_real_tweets[[k]]$detector,
               det_prob$event,
               numenta_realTweets$realTweets[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_cf_lr[[k]] <- NA
  }
}

names(ev_numenta_243_cf_lr) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_cf_lr[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_cf_lr <- append(acc_numenta_243_cf_lr, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "cf_lr_RealTweets",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_cf_lr)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_cf_lr[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_cf_lr <- append(f1_numenta_243_cf_lr, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "cf_lr_RealTweets",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


# #Method consolidation ---------------------------------------------------
acc_numenta_243_cf_lr
f1_numenta_243_cf_lr

prob_numenta_243_cons_overall$acc <- rbind(prob_numenta_243_cons_overall$acc,
                                           c("cf_lr",
                                             plim[i],
                                             mean(acc_numenta_243_cf_lr, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$acc) <- c("method", "plim", "value")

prob_numenta_243_cons_overall$f1 <- rbind(prob_numenta_243_cons_overall$f1,
                                          c("cf_lr",
                                            plim[i],
                                            mean(f1_numenta_243_cf_lr, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$f1) <- c("method", "plim", "value")

#LSTM =========
## art_anomaly -----------------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_art_anom)) {
  if(is.list(result_243_ml_lstm_numenta_art_anom[[k]])){
    prob <- result_243_ml_lstm_numenta_art_anom[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_art_anom[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_art_anom[[k]]$detector,
               det_prob$event,
               numenta_artificialWithAnomaly$artificialWithAnomaly[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_artificialWithAnomaly$artificialWithAnomaly)


## METRICS
#Accuracy
acc_numenta_243_ml_lstm <- c() #All series to create overall results


acc_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_ArtAnom",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_numenta_243_ml_lstm <- c() #All series to create overall results

f1_partial <- c()
j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_ArtAnom",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1

## REAL AD EXC -----------------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_real_adexc)) {
  if(is.list(result_243_ml_lstm_numenta_real_adexc[[k]])){
    prob <- result_243_ml_lstm_numenta_real_adexc[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_real_adexc[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_real_adexc[[k]]$detector,
               det_prob$event,
               numenta_realAdExchange$realAdExchange[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_realAdExchange$realAdExchange)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_ReadAdExch",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_ReadAdExch",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1



## REAL AWS -----------------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_real_aws)) {
  if(is.list(result_243_ml_lstm_numenta_real_aws[[k]])){
    prob <- result_243_ml_lstm_numenta_real_aws[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_real_aws[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_real_aws[[k]]$detector,
               det_prob$event,
               numenta_realAWSCloudwatch$realAWSCloudwatch[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_realAWSCloudwatch$realAWSCloudwatch)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_RealAWS",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_RealAWS",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Known Cause ----------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_real_knwcs)) {
  if(is.list(result_243_ml_lstm_numenta_real_knwcs[[k]])){
    prob <- result_243_ml_lstm_numenta_real_knwcs[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_real_knwcs[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_real_knwcs[[k]]$detector,
               det_prob$event,
               numenta_realKnownCause$realKnownCause[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_realKnownCause$realKnownCause)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_RealKnownCause",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_RealKnownCause",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Traffic ----------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_real_traff)) {
  if(is.list(result_243_ml_lstm_numenta_real_traff[[k]])){
    prob <- result_243_ml_lstm_numenta_real_traff[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_real_traff[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_real_traff[[k]]$detector,
               det_prob$event,
               numenta_realTraffic$realTraffic[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_realTraffic$realTraffic)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_RealTraffic",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_RealTraffic",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


## REAL Real Tweets ----------------------------------------------------
ev_numenta_243_ml_lstm <- list()


k = 1
for (k in 1:length(result_243_ml_lstm_numenta_real_tweets)) {
  if(is.list(result_243_ml_lstm_numenta_real_tweets[[k]])){
    prob <- result_243_ml_lstm_numenta_real_tweets[[k]]$prob
    prob_lim <- subset(prob, pe >= plim[i])
    
    det_prob <- result_243_ml_lstm_numenta_real_tweets[[k]]$detection
    det_prob$event <- 0
    det_prob$event[which(det_prob$idx %in% prob_lim$idx)] <- 1
    
    
    ev_numenta_243_ml_lstm[[k]] <- tryCatch({
      evaluate(result_243_ml_lstm_numenta_real_tweets[[k]]$detector,
               det_prob$event,
               numenta_realTweets$realTweets[[k]]$event)
    }, error = function(e) {
      message(e)
      return(NA)
    })
  } else {
    ev_numenta_243_ml_lstm[[k]] <- NA
  }
}

names(ev_numenta_243_ml_lstm) <- names(numenta_realTweets$realTweets)


## METRICS
#Accuracy
acc_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  acc_partial <- tryCatch({
    append(acc_partial, ev_numenta_243_ml_lstm[[j]]$accuracy)
  }, error = function(e) {
    message(e)
    return(append(acc_partial, NA))
  })
  acc_numenta_243_ml_lstm <- append(acc_numenta_243_ml_lstm, acc_partial)
}

prob_numenta_243_cons$acc <- rbind(prob_numenta_243_cons$acc,
                                   c(method = "ml_lstm_RealTweets",
                                     plim = plim[i],
                                     value = mean(acc_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$acc) <- c("method", "plim", "value")
prob_numenta_243_cons$acc


#F1
f1_partial <- c()

j = 1
for (j in 1:length(ev_numenta_243_ml_lstm)) {
  f1_partial <- tryCatch({
    append(f1_partial, ev_numenta_243_ml_lstm[[j]]$F1)
  }, error = function(e) {
    message(e)
    return(append(f1_partial, NA))
  })
  f1_numenta_243_ml_lstm <- append(f1_numenta_243_ml_lstm, f1_partial)
}

prob_numenta_243_cons$f1 <- rbind(prob_numenta_243_cons$f1,
                                  c(method = "ml_lstm_RealTweets",
                                    plim = plim[i],
                                    value = mean(f1_partial, na.rm = TRUE)))


names(prob_numenta_243_cons$f1) <- c("method", "plim", "value")
prob_numenta_243_cons$f1


# #Method consolidation ---------------------------------------------------
acc_numenta_243_ml_lstm
f1_numenta_243_ml_lstm

prob_numenta_243_cons_overall$acc <- rbind(prob_numenta_243_cons_overall$acc,
                                           c("ml_lstm",
                                             plim[i],
                                             mean(acc_numenta_243_ml_lstm, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$acc) <- c("method", "plim", "value")

prob_numenta_243_cons_overall$f1 <- rbind(prob_numenta_243_cons_overall$f1,
                                          c("ml_lstm",
                                            plim[i],
                                            mean(f1_numenta_243_ml_lstm, na.rm = TRUE)))
names(prob_numenta_243_cons_overall$f1) <- c("method", "plim", "value")


##=====
prob_numenta_243_cons
prob_numenta_243_cons_overall


## Save final results
save(prob_numenta_243_cons, file ="~/CEFET/nexus/consolidation/prob/prob_numenta_243_cons.RData", compress = TRUE)
save(prob_numenta_243_cons_overall, file ="~/CEFET/nexus/consolidation/prob/prob_numenta_243_cons_overall.RData", compress = TRUE)

## Load results
load(file ="~/CEFET/nexus/consolidation/prob/prob_numenta_243_cons.RData")
load(file ="~/CEFET/nexus/consolidation/prob/prob_numenta_243_cons_overall.RData")
