#########################################################
## GECCO 2018 - Build dataset (final package data)
## - Input: EventDetectR::geccoIC2018Train/Test
## - Output: harbinger/gecco.RData
## - Notes: reshapes into a per-series list with idx/value/event/type
#########################################################

build_gecco_dataset <- function() {
  # Source: GECCO Challenge 2018 (provided via EventDetectR)
  # This function builds a per-series list with an index column and
  # a unified logical event column, then saves it for package publication.

  # Dependencies are only required at runtime when generating data
  library(EventDetectR)

  # Combine train and test into a single data frame
  gecco_all <- rbind(geccoIC2018Train, geccoIC2018Test)

  # Remove time column and normalize event representation
  data <- subset(gecco_all, select = -c(Time))
  data$EVENT <- as.logical(as.integer(data$EVENT))

  # Organize each series (column) into a list entry with idx and event
  names(data) <- tolower(names(data))
  idx <- 1:nrow(data)
  out <- list()
  for (i in 1:(ncol(data) - 1)) {
    df <- data[, c(i, ncol(data))]
    colnames(df)[1] <- "value"
    out[[i]] <- cbind(data.frame(idx), df)
    names(out)[i] <- colnames(data)[i]
  }

  # Add multi-variate entry as convenience
  out$multi <- cbind(data.frame(idx), data)

  # Derive a human-readable type label from event
  for (i in 1:length(out)) {
    df <- out[[i]]
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
  }

  return(out)
}

# Build and persist dataset for the package (idempotent)
gecco <- build_gecco_dataset()
save(gecco, file = "harbinger/gecco.RData", compress = "xz")
