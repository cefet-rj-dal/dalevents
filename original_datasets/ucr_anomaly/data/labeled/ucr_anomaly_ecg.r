#Series selection ------------------
ecg_series_numbers<-c(11,12,13,14,15,16,17,18,55,56,57,58,87,88,96,104,119,120,
                  121,122,123,124,125,126,163,164,165,166,195,196,222,223,224,
                  225,226,227,228,229,230,231,232,233,234,235,236,237,238)

ucr_ecg <- ucr[ecg_series_numbers]

#Plot ------------------
for (i in 1:length(ucr_ecg)){
  plot(as.ts(ucr_ecg[[i]]),
       main=names(ucr_ecg[i]))
}

#Record ------------------
file <- "original_datasets/ucr_anomaly/data/labeled/ucr_ecg.RData"
save(ucr_ecg, file=file, compress = "xz")


#Sample by group
ecg_series_numbers <- c(11,12,96,104,119,120,163,164,222,223)

ecg_org_name <- c( "011_UCR_Anomaly_DISTORTEDECG1_10000_11800_12100.RData",
                   "012_UCR_Anomaly_DISTORTEDECG2_15000_16000_16100.RData",
                   "096_UCR_Anomaly_NOISEECG4_5000_16900_17100.RData",
                   "104_UCR_Anomaly_NOISEapneaecg4_6000_16000_16100.RData",
                   "119_UCR_Anomaly_ECG1_10000_11800_12100.RData",
                   "120_UCR_Anomaly_ECG2_15000_16000_16100.RData",
                   "163_UCR_Anomaly_apneaecg2_10000_20950_21100.RData",
                   "164_UCR_Anomaly_apneaecg3_5000_11111_11211.RData",
                   "222_UCR_Anomaly_mit14046longtermecg_56123_91200_91700.RData",
                   "223_UCR_Anomaly_mit14046longtermecg_74123_131200_131700.RData")

ecg_names <- c("011_ECG1","012_ECG2","096_ECG4","104_apneaecg4",
               "119_ECG1","120_ECG2","163_apneaecg2","164_apneaecg3",
               "222_mit14046longtermecg","223_mit14046longtermecg")



data <- ucr[ecg_series_numbers]
names(data) <- ecg_names

for (i in 1:length(data)){
  plot(as.ts(data[[i]]),
       main=names(data[i]))
}

ucr_ecg <- data
#Record ------------------
file <- "original_datasets/ucr_anomaly/data/labeled/ucr_ecg.RData"
save(ucr_ecg, file=file, compress = "xz")
