##==================================================================================
##
# Before using the label maker on the UCR file,
# you must read the full dataset documents.
# The way the UCR Anomaly Archive suggests event detection
# may be different from simply considering labels as real events.
#
#This script only labels the range proposed in the dataset documentation as events.
##
##==================================================================================

create_labels <- function(sample, tp = 1){
  #Automatically generates labels from a sample of the UCR Archive dataset
  #The variable with sample must have ucr_sample denomination
  require(stringr)

  #load(file)
  ucr_sample <- sample
  ucr_meta <- names(ucr_sample)

  ## Aplly label script to complete sample dataset
  j = 1
  for (j in 1:length(ucr_meta)) {
    parts <- str_split(ucr_meta[j], pattern = "_")[[1]]
    begin_anomaly <- as.integer(parts[6])
    end_anomaly <- as.integer(str_sub(parts[7], end = -7))
    tryCatch({
      x <- ucr_sample[[j]]
      if (is.vector(x) && !is.list(x)) {
        x <- data.frame(value = as.numeric(x))
      } else if (is.data.frame(x)) {
        if (ncol(x) >= 1) {
          names(x)[1] <- "value"
        }
      } else {
        x <- data.frame(value = as.numeric(x))
      }

      n <- nrow(x)
      if (is.na(begin_anomaly) || is.na(end_anomaly)) {
        stop("Invalid anomaly indices for ", ucr_meta[j])
      }
      begin_anomaly <- max(1, min(begin_anomaly, n))
      end_anomaly <- max(1, min(end_anomaly, n))

      x$event <- rep(FALSE, n)
      if (tp == 1) {
        x$event[begin_anomaly] <- TRUE
      } else {
        x$event[begin_anomaly:end_anomaly] <- TRUE
      }

      ucr_sample[[j]] <- x
    }, error = function(e){
      message(e)
    })
  }
  return(ucr_sample)
}
