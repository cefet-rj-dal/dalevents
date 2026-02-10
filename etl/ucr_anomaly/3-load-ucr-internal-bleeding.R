#########################################################
## UCR Anomaly Archive - Build InternalBleeding (final data)
## - Input: etl/ucr_anomaly/data/labeled/ucr_int_bleeding.RData
## - Output: harbinger/ucr_int_bleeding.RData
## - Notes: adds idx and type columns, normalizes structure
#########################################################

build_ucr_internal_bleeding <- function() {
  load(file.path("etl", "ucr_anomaly", "source", "ucr_int_bleeding.RData"))
  out <- list()
  for (i in seq_along(ucr_int_bleeding)) {
    idx <- 1:nrow(ucr_int_bleeding[[i]])
    df <- cbind(data.frame(idx), ucr_int_bleeding[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_int_bleeding)[i]
  }
  out
}

ucr_int_bleeding <- build_ucr_internal_bleeding()
save(ucr_int_bleeding, file = "harbinger/ucr_int_bleeding.RData", compress = "xz")

