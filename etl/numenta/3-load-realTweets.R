#########################################################
## NAB - Build realTweets (final package data)
## - Input: etl/numenta/source/realTweets/*.RData
## - Output: harbinger/nab_realTweets.RData
#########################################################

build_nab_realTweets <- function() {
  files <- list.files(path = "etl/numenta/source/realTweets", pattern = "\\.RData$", full.names = TRUE)
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

nab_realTweets <- build_nab_realTweets()
save(nab_realTweets, file = "harbinger/nab_realTweets.RData", compress = "xz")
