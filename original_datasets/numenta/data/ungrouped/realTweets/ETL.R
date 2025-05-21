carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  data <- nab_realTweets$realTweets

  #Transform boolean to integer to standardize column events with other datasets
  for (i in 1:length(data)){
    data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
  }

  #Dataset organized with series into a list
  nab_realTweets <- list()

  for (i in 1:(length(data))) {
    idx <- 1:nrow(data[[i]])
    nab_realTweets[[i]] <- cbind(data.frame(idx), data[[i]])
    names(nab_realTweets)[i] <- names(data)[i]
  }

  for (i in 1:length(nab_realTweets)){
    nab_realTweets[[i]]$type <- ""
    nab_realTweets[[i]]$type[nab_realTweets[[i]]$event] <- "anomaly"
  }

  return(nab_realTweets)
}

if (TRUE) {
  data(nab_realTweets)
  nab_realTweets <- carrega()
  save(nab_realTweets, file="data/nab_realTweets.RData", compress = "xz")
}
