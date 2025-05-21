carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- numenta_realKnownCause$realKnownCause

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_realKnownCause <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_realKnownCause[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_realKnownCause)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_realKnownCause)){
    nab_realKnownCause[[i]]$type <- ""
    nab_realKnownCause[[i]]$type[nab_realKnownCause[[i]]$event] <- "anomaly"
  }

  return(nab_realKnownCause)
}

if (TRUE) {
  data(numenta_realKnownCause)
  nab_realKnownCause <- carrega()
  save(nab_realKnownCause, file="data/nab_realKnownCause.RData", compress = "xz")
}
