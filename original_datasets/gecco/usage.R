#install.packages("strucchange")
#install.packages("TSA")
#install.packages("urca")
#install.packages("mFilter")

library(strucchange)
library(TSA)
library(urca)
library(mFilter)


#Load a series using files - Not recommended
#source("carrega.R")
#gecco <- carrega()

#out_file = "gecco.RData"
#save(gecco, file = out_file, compress = TRUE)


#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)

#Load a series using package - Recommended
#Load package
library(dalevents)

data(gecco)

#Select subset one day of pH series with synthetic events
test <- subset(gecco$gecco, select = c(ph, event))
test <- test[16500:18000,]
names(test) <- c("series", "event")

#Plotting original series
plot(ts(test$series),type="l",
     ylab="value",
     xlab="time")

#Autocorrelation function
plot(TSA::acf(ts(test$series), plot=FALSE, na.action = na.pass), main="")



#Stationarity analysis
#Unit root identification in the series
#ADF test
adf_res <- ur.df(ts(test$series),
                type = "trend",
                lags = 30,
                selectlags = "AIC"
                )

summary(adf_res)
#Compare test statistic values with critical values
#If value teststat > cval => Reject the null hypothesis ==> That is, the series is non-stationary
summary(adf_res)@teststat
summary(adf_res)@cval
