#########################################################
## Yahoo Webscope S5 - Build benchmarks (final data)
## - Input: etl/yahoo/source/A1Benchmark..A4Benchmark CSVs
## - Output: harbinger/A1Benchmark.RData..harbinger/A4Benchmark.RData
## - Notes: normalizes columns, builds event/type labels
#########################################################

library(stringr)
library(readr)
library(tools)

# Load a directory of 3-column CSVs (idx, value, event)
load_anomaly_dir <- function(dir) {
  out <- list()
  files <- list.files(path = dir, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) return(out)
  for (i in seq_along(files)) {
    tex <- files[[i]]
    data <- read_csv(tex, show_col_types = FALSE)
    colnames(data) <- c("idx", "value", "event")
    data$event <- as.logical(data$event)
    data$type <- ""
    data$type[data$event] <- "anomaly"
    out[[i]] <- data
    names(out)[i] <- tools::file_path_sans_ext(basename(tex))
  }
  out
}

# Load a directory of 4-column CSVs (idx, value, anomaly, cp) and fold into event/type
load_anomaly_cp_dir <- function(dir) {
  out <- list()
  files <- list.files(path = dir, pattern = ".csv$", full.names = TRUE, recursive = TRUE)
  if (length(files) == 0) return(out)
  for (i in seq_along(files)) {
    tex <- files[[i]]
    data <- read_csv(tex, show_col_types = FALSE)[, 1:4]
    colnames(data) <- c("idx", "value", "anomaly", "cp")
    data$anomaly <- as.logical(data$anomaly)
    data$cp <- as.logical(data$cp)
    data$event <- as.logical(data$anomaly | data$cp)
    data$type <- ""
    data$type[data$anomaly] <- "anomaly"
    data$type[data$cp] <- "changepoint"
    data$anomaly <- NULL
    data$cp <- NULL
    out[[i]] <- data
    names(out)[i] <- tools::file_path_sans_ext(basename(tex))
  }
  out
}

# Build and save datasets used by the package
A1Benchmark <- load_anomaly_dir("etl/yahoo/source/A1Benchmark")
save(A1Benchmark, file = "harbinger/A1Benchmark.RData", compress = "xz")

A2Benchmark <- load_anomaly_dir("etl/yahoo/source/A2Benchmark")
save(A2Benchmark, file = "harbinger/A2Benchmark.RData", compress = "xz")

A3Benchmark <- load_anomaly_cp_dir("etl/yahoo/source/A3Benchmark")
names(A3Benchmark) <- str_replace_all(names(A3Benchmark), "A3Benchmark-", "")
save(A3Benchmark, file = "harbinger/A3Benchmark.RData", compress = "xz")

A4Benchmark <- load_anomaly_cp_dir("etl/yahoo/source/A4Benchmark")
names(A4Benchmark) <- str_replace_all(names(A4Benchmark), "A4Benchmark-", "")
save(A4Benchmark, file = "harbinger/A4Benchmark.RData", compress = "xz")


