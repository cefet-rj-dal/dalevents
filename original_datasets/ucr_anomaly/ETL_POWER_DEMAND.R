carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- ucr_power_demand

  #Dataset organized with series into a list
  ucr_power_demand <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    ucr_power_demand[[i]] <- cbind(data.frame(idx), data[[i]])
    names(ucr_power_demand)[i] <- names(data)[i]
  }

  for (i in 1:length(ucr_power_demand)){
    ucr_power_demand[[i]]$type <- ""
    ucr_power_demand[[i]]$type[ucr_power_demand[[i]]$event] <- "anomaly"
  }

  return(ucr_power_demand)
}

if (TRUE) {
  load("~/cefet/dalevents/original_datasets/ucr_anomaly/data/labeled/ucr_power_demand.RData")
  ucr_power_demand <- carrega()
  save(ucr_power_demand, file="data/ucr_power_demand.RData", compress = "xz")
}
