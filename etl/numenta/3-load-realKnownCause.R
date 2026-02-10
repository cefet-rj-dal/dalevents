#########################################################
## NAB - Build realKnownCause (final package data)
## - Input: etl/numenta/source/realKnownCause/*.RData
## - Output: harbinger/nab_realKnownCause.RData
#########################################################

build_nab_realKnownCause <- function() {
  files <- list.files(path = "etl/numenta/source/realKnownCause", pattern = "\\.RData$", full.names = TRUE)
  out <- list()
  for (i in seq_along(files)) {
    load(files[i])
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

nab_realKnownCause <- build_nab_realKnownCause()
save(nab_realKnownCause, file = "harbinger/nab_realKnownCause.RData", compress = "xz")
