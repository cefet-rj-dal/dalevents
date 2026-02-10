#########################################################
## NAB - Build artificialWithAnomaly (final package data)
## - Input: etl/numenta/source/artificialWithAnomaly/*.RData
## - Output: harbinger/nab_artificialWithAnomaly.RData
#########################################################

build_nab_artificialWithAnomaly <- function() {
  files <- list.files(path = "etl/numenta/source/artificialWithAnomaly", pattern = "\\.RData$", full.names = TRUE)
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

nab_artificialWithAnomaly <- build_nab_artificialWithAnomaly()
save(nab_artificialWithAnomaly, file = "harbinger/nab_artificialWithAnomaly.RData", compress = "xz")
