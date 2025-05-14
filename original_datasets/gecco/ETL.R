carrega <- function() {
  library(EventDetectR)

  #Dataset origin
  gecco <- rbind(geccoIC2018Train, geccoIC2018Test)

  #Series without time column
  data <- subset(gecco, select = -c(Time))
  #Transform boolean to integer to standardize column events with other datasets
  data$EVENT <- as.logical(as.integer(data$EVENT))

  #Dataset organized with series into a list
  dataset <- data
  names(dataset) <- tolower(names(dataset))
  idx <- 1:nrow(dataset)
  gecco <- list()

  for (i in 1:(ncol(dataset)-1)) {
    data <- dataset[,c(i,ncol(dataset))]
    colnames(data)[1] <- "value"
    gecco[[i]] <- cbind(data.frame(idx), data)
    names(gecco)[i] <- colnames(dataset)[i]
  }

  gecco$multi <- cbind(data.frame(idx), dataset)

  for (i in 1:length(gecco)) {
    data <- gecco[[i]]
    data$type <- ""
    data$type[data$event] <- "anomaly"
    gecco[[i]] <- data
  }

  return(gecco)
}

if (TRUE) {
  gecco <- carrega()
  save(gecco, file="data/gecco.RData", compress = "xz")
}
