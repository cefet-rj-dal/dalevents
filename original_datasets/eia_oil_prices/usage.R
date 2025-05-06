#install.packages("strucchange")
#install.packages("TSA")
#install.packages("urca")
#install.packages("mFilter")

library(strucchange)
library(TSA)
library(urca)
library(mFilter)

#Load a series using files - Not recommended
#oil_file <- "eia_oil_prices/data/eia_oil_prices.RData"
#load(oil_file)
#crude_oil <- oil_prices_all$Crude_Oil

#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)

#Load a series using package - Recommended
#Load package
library(dalevents)

data(eia_oil_prices)

#Select subset crude oil prices
crude_oil <- eia_oil_prices$Crude_Oil
test <- crude_oil$EER_EPMRU_PF4_Y35NY_DPG
test <- na.omit(test)

#Plotting original series
plot(ts(test), type = "l",
     ylab="value",
     xlab = "time")

#Autocorrelation function
plot(TSA::acf(ts(test), plot=FALSE, na.action = na.pass), main="")


#Stationarity analysis
#Unit root identification in the series
#ADF test
adf_res <- ur.df(ts(test),
                 type = "trend",
                 lags = 30,
                 selectlags = "AIC"
)

summary(adf_res)
#Compare test statistic values with critical values
#If value teststat > cval => Reject the null hypothesis ==> That is, the series is non-stationary
summary(adf_res)@teststat
summary(adf_res)@cval
