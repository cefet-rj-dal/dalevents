carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- ucr_int_bleeding

  #Dataset organized with series into a list
  ucr_int_bleeding <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    ucr_int_bleeding[[i]] <- cbind(data.frame(idx), data[[i]])
    names(ucr_int_bleeding)[i] <- names(data)[i]
  }

  for (i in 1:length(ucr_int_bleeding)){
    ucr_int_bleeding[[i]]$type <- ""
    ucr_int_bleeding[[i]]$type[ucr_int_bleeding[[i]]$event] <- "anomaly"
  }

  return(ucr_int_bleeding)
}

if (TRUE) {
  load("~/cefet/dalevents/original_datasets/ucr_anomaly/data/labeled/ucr_int_bleeding.RData")
  ucr_int_bleeding <- carrega()
  save(ucr_int_bleeding, file="data/ucr_int_bleeding.RData", compress = "xz")
}
