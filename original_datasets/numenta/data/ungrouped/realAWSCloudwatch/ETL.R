carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- numenta_realAWSCloudwatch$realAWSCloudwatch

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_realAWSCloudwatch <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_realAWSCloudwatch[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_realAWSCloudwatch)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_realAWSCloudwatch)){
    nab_realAWSCloudwatch[[i]]$type <- ""
    nab_realAWSCloudwatch[[i]]$type[nab_realAWSCloudwatch[[i]]$event] <- "anomaly"
  }

  return(nab_realAWSCloudwatch)
}

if (TRUE) {
  data(numenta_realAWSCloudwatch)
  nab_realAWSCloudwatch <- carrega()
  save(nab_realAWSCloudwatch, file="data/nab_realAWSCloudwatch.RData", compress = "xz")
}
