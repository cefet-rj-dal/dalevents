# Labels Reference Datasets UCR Anomaly Archive ---------------------------
library(dalevents)
data(ucr)

serie <- ucr[[5]]
serie_meta <- names(serie)
serie_meta
#"DISTORTEDCIMIS44AirTemperature1_4000_5391_5392"
# Dataset number
# Mnemonic name --> DISTORTEDCIMIS44AirTemperature1
# From 1 to X is training data --> x = 4000
# Begin anomaly --> 5391
# End anomaly  --> 5392

serie$event <- rep(0, nrow(serie))

names(serie) <- c("series", "event")

serie$event[5391:5392]
serie$event[5391:5392] <- 1
sum(serie$event)

train_data <- serie[1:4000,]
test_data <- serie[4001:nrow(serie),]

plot(as.ts(serie$series))
plot(as.ts(train_data$series))
plot(as.ts(test_data$series))


serie <- ucr[[12]]
serie_meta <- names(serie)
serie_meta
#"DISTORTEDECG2_15000_16000_16100"
# Dataset number
# Mnemonic name --> DISTORTEDECG2
# From 1 to X is training data --> x = 15000
# Begin anomaly --> 16000
# End anomaly  --> 16100

serie$event <- rep(0, nrow(serie))

names(serie) <- c("series", "event")

serie$event[16000:16100]
serie$event[16000:16100] <- 1
sum(serie$event)

train_data <- serie[1:15000,]
test_data <- serie[15001:nrow(serie),]

plot(as.ts(serie$series))
plot(as.ts(train_data$series))
plot(as.ts(test_data$series))

##Apply in selected series

load("~/janio/harbinger/dev/ucr_sample.RData")
ucr_sample2 <- ucr_sample[-c(3)]

names(ucr_sample2)
ucr_meta <- names(ucr_sample2)
ucr_meta

ucr_meta[1]
#"240_UCR_Anomaly_taichidbS0715Master_240030_884100_884200.RData"
# Dataset number --> 240
# Mnemonic name --> taichidbS0715Master
# From 1 to X is training data --> x = 240030
# Begin anomaly --> 884100
# End anomaly  --> 884200
ucr_sample2[[1]]$event <- 0
names(ucr_sample2[[1]]) <- c("series", "event")
head(ucr_sample2[[1]])


ucr_sample2[[1]]$event[884100:884200]
ucr_sample2[[1]]$event[884100:884200] <- 1
sum(ucr_sample2[[1]]$event)

plot(as.ts(ucr_sample2[[1]]$series))
