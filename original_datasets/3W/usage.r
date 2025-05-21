#install.packages("strucchange")
#install.packages("TSA")
#install.packages("urca")
#install.packages("mFilter")

library(strucchange)
library(TSA)
library(urca)
library(mFilter)

#Load a series using files - Not recommended
#oil_well_3w_file <- "3w/data/grouped/oil_3w_Type_5.RData"
#load(oil_well_3w_file)

#Select subset T_TPT variable
#test <- oil_3w_grp$Type_5$`WELL-00016_20180426145108`


#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/event_datasets", force=TRUE)

#Load a series using package - Recommended
#Load package
library(dalevents)

data(oil_3w_Type_5)
test <- oil_3w_Type_5$Type_5$`WELL-00016_20180426145108`

test <- subset(test, select = c(T_TPT, class))
names(test) <- c("series", "event")

test <- na.omit(test)


#Plotting original series
plot(ts(test$series), type = "l",
     ylab="value",
     xlab = "time")

#Autocorrelation function
plot(TSA::acf(ts(test$series), plot=FALSE, na.action = na.pass), main="")


#Stationarity analysis
#Unit root identification in the series
#ADF test
adf_res <- ur.df(ts(test$series),
                 type = "trend",
                 lags = 30,
                 selectlags = "AIC")

summary(adf_res)
#Compare test statistic values with critical values
#If value teststat > cval => Reject the null hypothesis ==> That is, the series is non-stationary
summary(adf_res)@teststat
summary(adf_res)@cval
