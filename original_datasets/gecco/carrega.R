library(EventDetectR)
library(dplyr)


carrega <- function() {
  #Dataset origin
  gecco <- geccoIC2018Train
  
  #Series without time column
  data <- subset(gecco, select = -c(Time))
  #Transform boolean to integer to standardize column events with other datasets
  data$EVENT <- as.integer(data$EVENT)
  
  #Dataset organized with series into a list
  dataset <- list(data)
  names(dataset) <- "gecco"
  names(dataset$gecco) <- tolower(names(dataset$gecco))
  
  return(dataset)
}
