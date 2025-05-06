#NAB
# Statistics tests LSTM/GARCH ---------------------------------------------
## LAG ====

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


## TML ====
methods = c(rep("GARCH", length(na.omit(time_per_batch_numenta_243_garch))),
            rep("LSTM", length(na.omit(time_per_batch_numenta_243_ml_lstm))))


data_effect = data.frame(methods,
                         y = c(na.omit(time_per_batch_numenta_243_garch),
                               na.omit(time_per_batch_numenta_243_ml_lstm)))


wilcox_effsize(y ~ methods, paired=FALSE, data=data_effect)

## ACURÁCIA, F1, F1_SOFT ====
#acc
methods = c(rep("GARCH", length(na.omit(acc_numenta_243_garch))),
            rep("LSTM", length(na.omit(acc_numenta_243_ml_lstm))))


data_effect = data.frame(methods,
                         y = c(na.omit(acc_numenta_243_garch),
                               na.omit(acc_numenta_243_ml_lstm)))


wilcox_effsize(y ~ methods, paired=FALSE, data=data_effect)

#f1_soft
methods = c(rep("GARCH", length(na.omit(f1_soft_numenta_243_garch))),
            rep("LSTM", length(na.omit(f1_soft_numenta_243_ml_lstm))))


data_effect = data.frame(methods,
                         y = c(na.omit(f1_soft_numenta_243_garch),
                               na.omit(f1_soft_numenta_243_ml_lstm)))


wilcox_effsize(y ~ methods, paired=FALSE, data=data_effect)

