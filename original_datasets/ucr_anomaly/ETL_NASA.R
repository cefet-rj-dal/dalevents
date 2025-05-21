carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- ucr_nasa

  #Dataset organized with series into a list
  ucr_nasa <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    ucr_nasa[[i]] <- cbind(data.frame(idx), data[[i]])
    names(ucr_nasa)[i] <- names(data)[i]
  }

  for (i in 1:length(ucr_nasa)){
    ucr_nasa[[i]]$type <- ""
    ucr_nasa[[i]]$type[ucr_nasa[[i]]$event] <- "anomaly"
  }

  return(ucr_nasa)
}

if (TRUE) {
  load("~/cefet/dalevents/original_datasets/ucr_anomaly/data/labeled/ucr_nasa.RData")
  ucr_nasa <- carrega()
  save(ucr_nasa, file="data/ucr_nasa.RData", compress = "xz")
}
