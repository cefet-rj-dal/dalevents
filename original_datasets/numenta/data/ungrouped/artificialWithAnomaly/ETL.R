carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- numenta_artificialWithAnomaly$artificialWithAnomaly

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_artificialWithAnomaly <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_artificialWithAnomaly[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_artificialWithAnomaly)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_artificialWithAnomaly)){
    nab_artificialWithAnomaly[[i]]$type <- ""
    nab_artificialWithAnomaly[[i]]$type[nab_artificialWithAnomaly[[i]]$event] <- "anomaly"
  }

  return(nab_artificialWithAnomaly)
}

if (TRUE) {
  data(numenta_artificialWithAnomaly)
  nab_artificialWithAnomaly <- carrega()
  save(nab_artificialWithAnomaly, file="data/nab_artificialWithAnomaly.RData", compress = "xz")
}
