#install.packages("strucchange")
#install.packages("TSA")
#install.packages("urca")
#install.packages("mFilter")

library(strucchange)
library(TSA)
library(urca)
library(mFilter)
library(dplyr)

#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)

#Load a series using package - Recommended
#Load package
library(dalevents)

data(mortality_cnes)


#Filter one counties and two variables
mortality_sp <- mortality_cnes %>%
  group_by(mun_MUNNOME) %>%
  filter(uf_SIGLA_UF == "SP") %>%
  summarize(neo_Obitos = sum(neo_Obitos, na.rm = TRUE))


mortality_sp <- mortality_cnes %>%
  filter(uf_SIGLA_UF == "SP")

mortality_sp_cidade <- mortality_sp %>%
  filter(mun_MUNNOME == "São Paulo")

mortality_sp_cidade <- subset(mortality_sp_cidade, select = c(Nascimentos, neo_Obitos))

#Plotting data
plot(ts(mortality_sp_cidade), type = "l")


##ATENTION:
#The analysis and tests below may not be adequate
#Before using them, a prior understanding of the data is necessary
#The best use is when the data is already modeled as a time series

#Autocorrelation function
plot(TSA::acf(ts(mortality_sp_cidade$neo_Obitos), plot=FALSE, na.action = na.pass), main="")

#Stationarity analysis
#Unit root identification in the series
#ADF test
adf_res <- ur.df(ts(mortality_sp_cidade$neo_Obitos),
                 type = "trend",
                 lags = 30,
                 selectlags = "AIC")

summary(adf_res)
#Compare test statistic values with critical values
#If value teststat > cval => Reject the null hypothesis ==> That is, the series is non-stationary
summary(adf_res)@teststat
summary(adf_res)@cval
