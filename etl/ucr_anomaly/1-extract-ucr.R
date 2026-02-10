#########################################################
## UCR Anomaly Archive - ETL for package data
## - Zip original TXT series into RData
## - Build a grouped list object for package publishing
#########################################################

library(stringr)
library(readr)

series_root <- "etl/ucr_anomaly"

# Zip original data files as .RData
zip_dataset <- function(series_root) {
  src_dir <- file.path(series_root, "original")
  files_sr <- list.files(path = src_dir, pattern = "\\.txt$", full.names = TRUE)
  for (series_file in files_sr) {
    series <- read_csv(series_file, col_names = FALSE, show_col_types = FALSE)
    nm <- str_sub(series_file, start = nchar(file.path(series_root, "original")) + 2, end = -5)
    names(series) <- c(nm)
    out_file <- file.path(series_root, "zip", paste0(str_sub(series_file, start = nchar(series_root) + 2, end = -5), ".RData"))
    dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
    save(series, file = out_file, compress = TRUE)
  }
}

# Create the complete dataset organized as a list
create_grouped <- function() {
  dataset <- list()
  series_path <- file.path(series_root, "intermediate", "zip")
  files_sr <- list.files(path = series_path, pattern = "\\.RData$")
  for (k in seq_along(files_sr)) {
    series_file <- file.path(series_path, files_sr[k])
    load(series_file)
    dataset[[k]] <- series
  }
  names(dataset) <- files_sr
  dataset
}

# Build artifacts
zip_dataset(series_root)
ucr <- create_grouped()
dir.create(file.path(series_root, "intermediate", "grouped"), recursive = TRUE, showWarnings = FALSE)
save(ucr, file = file.path(series_root, "intermediate", "grouped", "ucr.RData"), compress = TRUE)
