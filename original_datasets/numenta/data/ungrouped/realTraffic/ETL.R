carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- nab_realTraffic$realTraffic

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_realTraffic <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_realTraffic[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_realTraffic)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_realTraffic)){
    nab_realTraffic[[i]]$type <- ""
    nab_realTraffic[[i]]$type[nab_realTraffic[[i]]$event] <- "anomaly"
  }

  return(nab_realTraffic)
}

if (TRUE) {
  data(nab_realTraffic)
  nab_realTraffic <- carrega()
  save(nab_realTraffic, file="data/nab_realTraffic.RData", compress = "xz")
}
