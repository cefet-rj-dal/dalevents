#Starting nexus environment (load nexus, dal tool box, harbinger and datasets)
source("~/janio/nexus/dev/env_start.R")


load(file="~/janio/harbinger/dev/results/result_243_fbiad_numenta_art_anom.RData")
data("numenta_artificialWithAnomaly")

#Example 1: Evaluation of a specif time point
stream_evaluate(time=84,
             nexus_result = result_243_fbiad_numenta_art_anom$art_daily_flatmidd)


#Example 2: Evaluation of a specifc time points from a vector
idx = c(84, 85, 100)
stream_evaluate(time=idx[1],
             result_243_fbiad_numenta_art_anom$art_daily_flatmidd)

stream_evaluate(time=idx[3],
                result_243_fbiad_numenta_art_anom$art_daily_flatmidd)

#Example 3: Default complete result lag evaluation
str <- stream_evaluate(time=0,
             result_243_fbiad_numenta_art_anom$art_daily_flatmidd)

head(str)
View(str)


# Draft - Do not run ------------------------------------------------------
head(res$prob)

stream_res <- res$prob
#sb_t <- ceiling(pos / (bsz+wsz))
#wsz <- nexus_result$warm_size
#bsz <- nexus_result$batch_size
#lag_t = fdb_t - sb_t
stream_res$sb <- ceiling(stream_res$idx / (res$warm_size + res$batch_size))
stream_res$lag <- stream_res$fdb - stream_res$sb

stream_res
head(stream_res)
View(stream_res)

mean(stream_res$lag)

plot(as.ts(stream_res$lag),
     ylab = "Lag")
lines(x = c(1:nrow(stream_res)), y = rep(mean(stream_res$lag), nrow(stream_res)), 
      lty = 2, #Tipo de linha
      lwd = 1, #Tipo de linha
      col="red")
legend(x="topleft",
       legend = "Average lag",
       lty = 2, lwd = 1,
       bty = "n",
       col="red")


plim = 0.8
stream_res_prob <- res$prob[res$prob$pe > plim,]



# Lag analysis ------------------------------------------------------------
#Formula: Lag_t = fdb_t - sb_t, t => Position of event to analysis
#Real events indexes
idx_reference <- which(data$event == 1)
idx_reference


#Multiple events analysis
#Don't repeat sample line to avoid wrong experiments reproduction
#rdn <- sample(1:length(idx_reference), 3)

#Random numbers --> 12  1 41
#Line added to maintain the original random numbers results
rdn <- c(12,1,41)

#Original reference events index --> 67  56 770
positions <- idx_reference[rdn]
positions


#56 equivale ao primeiro evento da série
#Adicionar último evento da série 
last_event <- idx_reference[length(idx_reference)]
positions <- append(positions, last_event)

positions

## Develop and test --------------------------
#Batch size
bsz <- result_fbiad_ph_mem$batch_size
bsz

#Position of event to analysis
pos <- idx_reference[1]
pos

pos_result <- result_fbiad_ph_mem$prob[result_fbiad_ph_mem$prob$idx == pos,]
pos_result

fdb_pos <- pos_result$fdb
fdb_pos

sb_pos = ceiling(pos / bsz)

lag_pos = fdb_pos - sb_pos
lag_pos


pos / bsz

ceiling(242 / bsz)
ceiling(243 / bsz)
ceiling(244 / bsz)
ceiling(500 / bsz)

#Function to lag evaluate - only for dev =================
#To use in experiments call it directly from nexus.


#' @name lag_evaluate
#' @title Nexus Lag Evaluate lag detection on stream event detection
#' @description Evaluate lag detection on stream event detection
#'
#' @docType function
#' @usage lag_evaluate(time, nexus_result)
#' @format A data frame with Stream Result or individual lag results
#' @keywords lag
#'
#'
#' @examples
#' stream_result <- lag_evaluate(time=0, nexus_result=fbiad_result)
#' #time = 0 --> Return complete stream result
#' #time = t > 0 --> Return lag analysis to event detected on time t
#' 
lag_evaluate <- function(time=0, nexus_result) {
  mt <- class(nexus_result$detector)[1]
  if (time > 0) {
    #Result info collect
    t_res_info <- nexus_result$prob[nexus_result$prob$idx == time,]
    wsz <- nexus_result$warm_size
    bsz <- nexus_result$batch_size
    #Formula parameters
    sb_t <- ceiling(time / (bsz+wsz))
    
    if (nrow(t_res_info) > 0) {
      #Formula parameters
      fdb_t <- t_res_info$fdb
      #Lag calculation
      lag_t = fdb_t - sb_t
      #Return lag analysis
      lag_ev <- data.frame(idx = time, fdb = fdb_t, sb = sb_t,
                           lag = lag_t, batch_size = bsz, method = mt)
      
      return(lag_ev)
    }
    #Return info about not lag available
    lag_ev <- data.frame(idx = time, fdb = NA, sb = sb_t,
                         lag = NA, batch_size = bsz, method = mt)
    return(lag_ev)
  }
  stream_res <- nexus_result$prob
  stream_res$sb <- ceiling(stream_res$idx / (res$warm_size + res$batch_size))
  stream_res$lag <- stream_res$fdb - stream_res$sb
  stream_res$method <- mt
  return(stream_res)
}



## Using stable lag calculation --------------------------
#Individual analysis
lag_ph_mem3_cons <- data.frame(lag_evaluate(pos=pos, nexus_result = result_fbiad_ph_mem, reference = data$event))
lag_ph_mem3_cons <- rbind(lag_ph_mem3_cons, lag_evaluate(pos=pos, nexus_result = result_arima_ph_mem, reference = data$event))
lag_ph_mem3_cons <- rbind(lag_ph_mem3_cons, lag_evaluate(pos=pos, nexus_result = result_garch_ph_mem, reference = data$event))
lag_ph_mem3_cons <- rbind(lag_ph_mem3_cons, lag_evaluate(pos=pos, nexus_result = result_cf_lr_ph_mem, reference = data$event))
lag_ph_mem3_cons <- rbind(lag_ph_mem3_cons,lag_evaluate(pos=pos, nexus_result = result_ml_lstm_ph_mem, reference = data$event))



#Cons analysis w=243, s=243, m=3 -----

j = 1
for (j in 1:length(positions)) {
  if (j == 1) {
    #Creation and calculate first position lag
    lag_ph_mem3_cons_mult <- data.frame(lag_evaluate(pos=positions[j],
                                                     nexus_result = result_fbiad_ph_mem,
                                                     reference = data$event))
    #Add lines to first positions and other methods
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_arima_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_garch_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_cf_lr_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_ml_lstm_ph_mem,
                                                reference = data$event))
  } else {
    #Add lines to other postions lag
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_fbiad_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_arima_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_garch_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_cf_lr_ph_mem,
                                                reference = data$event))
    lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_ml_lstm_ph_mem,
                                                reference = data$event))
    
  }
}

lag_ph_mem3_cons_mult


lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                               lag_evaluate(pos=last_event,
                                            nexus_result = result_fbiad_ph_mem,
                                            reference = data$event))

lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                               lag_evaluate(pos=last_event,
                                            nexus_result = result_arima_ph_mem,
                                            reference = data$event))

lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                               lag_evaluate(pos=last_event,
                                            nexus_result = result_garch_ph_mem,
                                            reference = data$event))

lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                               lag_evaluate(pos=last_event,
                                            nexus_result = result_cf_lr_ph_mem,
                                            reference = data$event))

lag_ph_mem3_cons_mult <- rbind(lag_ph_mem3_cons_mult,
                               lag_evaluate(pos=last_event,
                                            nexus_result = result_ml_lstm_ph_mem,
                                            reference = data$event))

lag_ph_mem3_cons_mult


save(lag_ph_mem3_cons_mult, file = "~/janio/harbinger/dev/results/lag_ph_mem3_cons_mult.RData", compress = TRUE)
load(file = "~/janio/harbinger/dev/results/lag_ph_mem3_cons_mult.RData")



#Cons analysis w=243, s=243, m=0 ----

#Multiple events analysis

j = 1
for (j in 1:length(positions)) {
  if (j == 1) {
    #Creation and calculate first position lag
    lag_ph_mem0_cons_mult <- data.frame(lag_evaluate(pos=positions[j],
                                                     nexus_result = result_fbiad_ph,
                                                     reference = data$event))
    #Add lines to first positions and other methods
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_arima_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_garch_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_cf_lr_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_ml_lstm_ph,
                                                reference = data$event))
  } else {
    #Add lines to other postions lag
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_fbiad_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_arima_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_garch_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_cf_lr_ph,
                                                reference = data$event))
    lag_ph_mem0_cons_mult <- rbind(lag_ph_mem0_cons_mult,
                                   lag_evaluate(pos=positions[j],
                                                nexus_result = result_ml_lstm_ph,
                                                reference = data$event))
    
  }
}

lag_ph_mem0_cons_mult

save(lag_ph_mem0_cons_mult, file = "~/janio/harbinger/dev/results/lag_ph_mem0_cons_mult.RData", compress = TRUE)
load(file = "~/janio/harbinger/dev/results/lag_ph_mem0_cons_mult.RData")




# Examples STR complete ---------------------------------------------------
#NAB
plot(as.ts(str_numenta_garch$real_adexc$`exchange-3_cpm_resul`[c(4,3,5,2)]),
     main="Stream Result: GARCH - NAB - Real Ad Exchange - Random series",
     xlab = "Events")


plot(as.ts(str_numenta_garch$real_adexc$`exchange-2_cpm_resul`[c(4,3,5,2)]),
     main="",
     xlab = "Events NAB - Real Ad Exchange")


plot(as.ts(str_numenta_garch$real_knwcs$machine_temperature_system_failu[c(4,3,5,2)]),
     main="Stream Result: GARCH - NAB - Real Known Cause - Random Series",
     xlab = "Events")


plot(as.ts(str_numenta_garch$real_adexc$`exchange-3_cpm_resul`[c(4,3,5,2)]),
     main="",
     xlab = "Events NAB - Real Ad Exchange")

plot(as.ts(str_numenta_garch$real_knwcs$machine_temperature_system_failu[c(4,3,5,2)]),
     main="",
     xlab = "Events NAB - Real Known Cause")


plot(as.ts(str_numenta_lstm$real_adexc$`exchange-2_cpc_resul`[c(4,3,5,2)]),
     main="",
     xlab = "Events NAB - Real Ad Exchange"
     )


#RARE
plot(as.ts(str_rare_garch$kafka_server_brokertopicmetrics_totalfetchrequestspersec_oneminuterate_1[c(4,3,5,2)]),
     main="",
     xlab = "Events RARE - Kafka server topics")



# Statistics tests LSTM/GARCH ---------------------------------------------
library(ggplot2)

dt <- data.frame(method = "GARCH",
                 lag=str_numenta_garch$real_adexc$`exchange-2_cpm_resul`$lag)

dt <- rbind(dt, data.frame(method = "LSTM",
                           lag=str_numenta_lstm$real_adexc$`exchange-2_cpc_resul`$lag))
dt



ggplot(data=dt, aes(x=method, y=lag)) +
  stat_boxplot(geom = "errorbar", width = .33) +
  geom_boxplot(width = .66, col="black") +
  xlab("Métodos") +
  ylab("Lag")


#Realizando teste de normalidade usando Shapiro-Wilk
#Hipotese nula: não há evidência que a distribuição não seja normal (p-value >= 0.05)
#Hipotese alternativa: a distribuição não é normal (p-value < 0.05)

#GARCH p-value = 3.376e-13 -> REJEITA hipótese nula
shapiro.test(dt$lag[dt$method=="GARCH"])

#LSTM p-value = 5.268e-14 -> REJEITA a hipótese nula
shapiro.test(dt$lag[dt$method=="LSTM"])


#Ambas as distribuições dos tempos estão condizentes com a hipótese nula
#Teste secundário
library(nortest)
ad.test(dt$lag[dt$method=="GARCH"])
ad.test(dt$lag[dt$method=="LSTM"])


#Prosseguir com análise NÃO paramétrica
length(dt$lag[dt$method=="LSTM"])
length(dt$lag[dt$method=="GARCH"])

st_test <- wilcox.test(dt$lag[dt$method=="GARCH"],
                       dt$lag[dt$method=="LSTM"],
                       paired = FALSE,
                       exact = FALSE)
st_test

library(rstatix)
#Comparison MC
methods = c(rep("GARCH", length(dt$lag[dt$method=="GARCH"])),
            rep("LSTM", length(dt$lag[dt$method=="LSTM"])))

data_effect = data.frame(methods,
                         y = c(dt$lag[dt$method=="GARCH"],
                               dt$lag[dt$method=="LSTM"]))


wilcox_effsize(y ~ methods, paired=FALSE, data=data_effect)

# A tibble: 1 × 7
#.y.   group1 group2 effsize    n1    n2 magnitude
#* <chr> <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 y     GARCH  LSTM     0.100   152   197 small 


#Comparison general
dt_overall <- data.frame(method = "GARCH",
                 lag=lag_numenta_garch)

dt_overall <- rbind(dt_overall, data.frame(method = "LSTM",
                           lag=lag_numenta_lstm))
dt_overall


ggplot(data=dt_overall, aes(x=method, y=lag)) +
  stat_boxplot(geom = "errorbar", width = .33) +
  geom_boxplot(width = .66, col="black") +
  xlab("Métodos") +
  ylab("Lag")


methods = c(rep("GARCH", length(na.omit(lag_numenta_garch))),
            rep("LSTM", length(na.omit(lag_numenta_lstm))))


data_effect = data.frame(methods,
                         y = c(na.omit(lag_numenta_garch),
                               na.omit(lag_numenta_lstm)))


wilcox_effsize(y ~ methods, paired=FALSE, data=data_effect)

# A tibble: 1 × 7
#.y.   group1 group2 effsize    n1    n2 magnitude
#* <chr> <chr>  <chr>    <dbl> <int> <int> <ord>    
#  1 y     GARCH  LSTM    0.0542  3306 16017 small


