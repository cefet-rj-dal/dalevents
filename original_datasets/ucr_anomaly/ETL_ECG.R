carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- ucr_ecg

  #Transform boolean to integer to standardize column events with other datasets
  #for (i in 1:length(data)){
  #  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  #}

  #Dataset organized with series into a list
  ucr_ecg <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    ucr_ecg[[i]] <- cbind(data.frame(idx), data[[i]])
    names(ucr_ecg)[i] <- names(data)[i]
  }

  for (i in 1:length(ucr_ecg)){
    ucr_ecg[[i]]$type <- ""
    ucr_ecg[[i]]$type[ucr_ecg[[i]]$event] <- "anomaly"
  }

  return(ucr_ecg)
}

if (TRUE) {
  load("~/cefet/dalevents/original_datasets/ucr_anomaly/data/labeled/ucr_ecg.RData")
  ucr_ecg <- carrega()
  save(ucr_ecg, file="data/ucr_ecg.RData", compress = "xz")
}
