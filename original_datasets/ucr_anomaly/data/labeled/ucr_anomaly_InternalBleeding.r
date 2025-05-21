load("original_datasets/ucr_anomaly/data/labeled/ucr.RData")

#Sample by group
int_bleeding_series_numbers <- c(24,25,26,27,98,99,132,133,134,135)

int_bleeding_names <- c("024_InternalBleeding10","025_InternalBleeding14",
                        "026_InternalBleeding15","027_InternalBleeding16",
                        "098_InternalBleeding16","099_InternalBleeding6",
                        "132_InternalBleeding10","133_InternalBleeding14",
                        "134_InternalBleeding15","135_InternalBleeding16")



data <- ucr[int_bleeding_series_numbers]
names(data) <- int_bleeding_names

for (i in 1:length(data)){
  plot(as.ts(data[[i]]),
       main=names(data[i]))
}

ucr_int_bleeding <- data
#Record ------------------
file <- "original_datasets/ucr_anomaly/data/labeled/ucr_int_bleeding.RData"
save(ucr_int_bleeding, file=file, compress = "xz")
