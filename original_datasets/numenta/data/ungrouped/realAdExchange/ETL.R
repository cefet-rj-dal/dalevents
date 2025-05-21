carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- numenta_realAdExchange$realAdExchange

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_realAdExchange <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_realAdExchange[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_realAdExchange)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_realAdExchange)){
    nab_realAdExchange[[i]]$type <- ""
    nab_realAdExchange[[i]]$type[nab_realAdExchange[[i]]$event] <- "anomaly"
  }

  return(nab_realAdExchange)
}

if (TRUE) {
  data(numenta_realAdExchange)
  nab_realAdExchange <- carrega()
  save(nab_realAdExchange, file="data/nab_realAdExchange.RData", compress = "xz")
}
