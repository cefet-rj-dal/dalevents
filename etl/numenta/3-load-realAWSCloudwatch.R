#########################################################
## NAB - Build realAWSCloudwatch (final package data)
## - Input: etl/numenta/source/realAWSCloudwatch/*.RData
## - Output: harbinger/nab_realAWSCloudwatch.RData
#########################################################

build_nab_realAWSCloudwatch <- function() {
  files <- list.files(path = "etl/numenta/source/realAWSCloudwatch", pattern = "\\.RData$", full.names = TRUE)
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

nab_realAWSCloudwatch <- build_nab_realAWSCloudwatch()
save(nab_realAWSCloudwatch, file = "harbinger/nab_realAWSCloudwatch.RData", compress = "xz")
