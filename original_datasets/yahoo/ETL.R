library(stringr)
library(readr)
library(tools)

carrega_anomaly <- function(dir) {
  dataset <- list()
  i <- 0
  texs <- list.files(path = dir, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  if (TRUE) {
    for (tex in texs) {
      i <- i + 1
      data <- read_csv(tex)
      colnames(data) <- c("idx", "value", "event")
      data$event <- as.logical(data$event)
      data$type <- ""
      data$type[data$event] <- "anomaly"
      dataset[[i]] <- data
      names(dataset)[i] <- tools::file_path_sans_ext(basename(tex))
    }
  }
  return(dataset)
}

carrega_anomaly_cp <- function(dir) {
  dataset <- list()
  i <- 0
  texs <- list.files(path = dir, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  if (TRUE) {
    for (tex in texs) {
      i <- i + 1
      data <- read_csv(tex)[,1:4]
      colnames(data) <- c("idx", "value", "anomaly", "cp")
      data$anomaly <- as.logical(data$anomaly)
      data$cp <- as.logical(data$cp)
      data$event <- as.logical(data$anomaly | data$cp)
      data$type <- ""
      data$type[data$anomaly] <- "anomaly"
      data$type[data$cp] <- "changepoint"
      data$anomaly <- NULL
      data$cp <- NULL

      dataset[[i]] <- data
      names(dataset)[i] <- tools::file_path_sans_ext(basename(tex))
    }
  }
  return(dataset)
}

if (TRUE) {
  A1Benchmark <- carrega_anomaly("original_datasets/yahoo/A1Benchmark")
  save(A1Benchmark, file="data/A1Benchmark.RData", compress = "xz")
}


if (TRUE) {
  A2Benchmark <- carrega_anomaly("original_datasets/yahoo/A2Benchmark")
  save(A2Benchmark, file="data/A2Benchmark.RData", compress = "xz")
}

if (TRUE) {
  A3Benchmark <- carrega_anomaly_cp("original_datasets/yahoo/A3Benchmark")
  names(A3Benchmark) <- str_replace_all(names(A3Benchmark), "A3Benchmark-", "")
  save(A3Benchmark, file="data/A3Benchmark.RData", compress = "xz")
}

if (TRUE) {
  A4Benchmark <- carrega_anomaly_cp("original_datasets/yahoo/A4Benchmark")
  names(A4Benchmark) <- str_replace_all(names(A4Benchmark), "A4Benchmark-", "")
  save(A4Benchmark, file="data/A4Benchmark.RData", compress = "xz")
}


