#########################################################
## UCR Anomaly Archive - Build ECG (final package data)
## - Input: etl/ucr_anomaly/source/ucr_ecg.RData
## - Output: harbinger/ucr_ecg.RData
## - Notes: adds idx and type columns, normalizes structure
#########################################################

build_ucr_ecg <- function() {
  # Load labeled ECG series and standardize structure for package data
  load(file.path("etl", "ucr_anomaly", "source", "ucr_ecg.RData"))
  out <- list()
  for (i in seq_along(ucr_ecg)) {
    idx <- 1:nrow(ucr_ecg[[i]])
    df <- cbind(data.frame(idx), ucr_ecg[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_ecg)[i]
  }
  out
}

ucr_ecg <- build_ucr_ecg()
save(ucr_ecg, file = "harbinger/ucr_ecg.RData", compress = "xz")
