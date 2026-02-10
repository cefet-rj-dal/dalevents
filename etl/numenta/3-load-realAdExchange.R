#########################################################
## NAB - Build realAdExchange (final package data)
## - Input: etl/numenta/source/realAdExchange/*.RData
## - Output: harbinger/nab_realAdExchange.RData
#########################################################

build_nab_realAdExchange <- function() {
  files <- list.files(path = "etl/numenta/source/realAdExchange", pattern = "\\.RData$", full.names = TRUE)
  out <- list()
  for (i in seq_along(files)) {
    load(files[i]) # loads 'series'
    df <- series
    df$event <- as.logical(df$event)
    idx <- 1:nrow(df)
    df <- cbind(data.frame(idx), df)
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    nm <- tools::file_path_sans_ext(basename(files[i]))
    names(out)[i] <- nm
  }
  out
}

nab_realAdExchange <- build_nab_realAdExchange()
save(nab_realAdExchange, file = "harbinger/nab_realAdExchange.RData", compress = "xz")
