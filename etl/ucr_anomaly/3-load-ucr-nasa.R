#########################################################
## UCR Anomaly Archive - Build NASA (final package data)
## - Input: etl/ucr_anomaly/source/ucr_nasa.RData
## - Output: harbinger/ucr_nasa.RData
## - Notes: adds idx and type columns, normalizes structure
#########################################################

build_ucr_nasa <- function() {
  load(file.path("etl", "ucr_anomaly", "source", "ucr_nasa.RData"))
  out <- list()
  for (i in seq_along(ucr_nasa)) {
    idx <- 1:nrow(ucr_nasa[[i]])
    df <- cbind(data.frame(idx), ucr_nasa[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_nasa)[i]
  }
  out
}

ucr_nasa <- build_ucr_nasa()
save(ucr_nasa, file = "harbinger/ucr_nasa.RData", compress = "xz")
