#########################################################
## UCR Anomaly Archive - Build power_demand (final data)
## - Input: etl/ucr_anomaly/source/ucr_power_demand.RData
## - Output: harbinger/ucr_power_demand.RData
## - Notes: adds idx and type columns, normalizes structure
#########################################################

build_ucr_power_demand <- function() {
  load(file.path("etl", "ucr_anomaly", "source", "ucr_power_demand.RData"))
  out <- list()
  for (i in seq_along(ucr_power_demand)) {
    idx <- 1:nrow(ucr_power_demand[[i]])
    df <- cbind(data.frame(idx), ucr_power_demand[[i]])
    df$type <- ""
    df$type[df$event] <- "anomaly"
    out[[i]] <- df
    names(out)[i] <- names(ucr_power_demand)[i]
  }
  out
}

ucr_power_demand <- build_ucr_power_demand()
save(ucr_power_demand, file = "harbinger/ucr_power_demand.RData", compress = "xz")
