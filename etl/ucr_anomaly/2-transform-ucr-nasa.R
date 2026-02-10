#########################################################
## UCR Anomaly Archive - Build NASA subset
## - Input: etl/ucr_anomaly/source/ucr.RData
## - Output: etl/ucr_anomaly/source/ucr_nasa.RData
## - Notes: selects NASA series and saves labeled subset
#########################################################

load("etl/ucr_anomaly/source/ucr.RData")

#Sample by group
nasa_series_numbers <- c(48,49,50,51,52,103,156,157,158,159,160)

nasa_names <- c("048_TkeepFifthMARS","049_TkeepFirstMARS","050_TkeepForthMARS",
               "051_TkeepSecondMARS","052_TkeepThirdMARS","103_TkeepThirdMARS",
               "156_TkeepFifthMARS","157_TkeepFirstMARS","158_TkeepForthMARS",
               "159_TkeepSecondMARS","160_TkeepThirdMARS")



data <- ucr[nasa_series_numbers]
names(data) <- nasa_names

ucr_nasa <- data
#Record ------------------
file <- "etl/ucr_anomaly/source/ucr_nasa.RData"
save(ucr_nasa, file=file, compress = "xz")
