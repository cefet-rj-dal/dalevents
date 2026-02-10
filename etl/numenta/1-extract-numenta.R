#########################################################
## Numenta - Time Series Event Benchmark: ETL
## - Zip original CSVs
## - Attach labels (where available) to build ungrouped series
## - Build grouped list objects for package data
#########################################################

library(readr)
library(stringr)

series_description <- c(
  "artificialNoAnomaly",
  "artificialWithAnomaly",
  "realAdExchange",
  "realAWSCloudwatch",
  "realKnownCause",
  "realTraffic",
  "realTweets"
)
series_path <- series_description

# 1) Zip original CSVs into RData
zip_dataset <- function(groups) {
  for (grp in groups) {
    files_sr <- list.files(path = file.path("etl", "numenta", "original", grp), pattern = "\\.csv$", full.names = TRUE)
    for (series_file in files_sr) {
      series <- read_csv(series_file, show_col_types = FALSE)
      rel <- str_sub(series_file, start = nchar(file.path("etl", "numenta", "original")) + 2, end = -5)
      out_file <- file.path("etl", "numenta", "intermediate", "zip", paste0(rel, ".RData"))
      dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
      save(series, file = out_file, compress = TRUE)
    }
  }
}

zip_dataset(series_path)

# 2) Attach labels to zipped series and create ungrouped series
lab_path <- c(
  "artificialWithAnomaly",
  "realAdExchange",
  "realAWSCloudwatch",
  "realKnownCause",
  "realTraffic",
  "realTweets"
)

for (grp in lab_path) {
  files_lb <- list.files(path = file.path("etl", "numenta", "labels", grp), pattern = "\\.Rds$", full.names = TRUE)
  files_zip <- list.files(path = file.path("etl", "numenta", "intermediate", "zip", grp), pattern = "\\.RData$", full.names = TRUE)
  # Sort to improve alignment between zip and label files if names match
  files_lb <- sort(files_lb)
  files_zip <- sort(files_zip)
  for (i in seq_along(files_lb)) {
    lb <- readRDS(files_lb[i])
    names(lb) <- c("time", "event")
    load(files_zip[i]) # loads 'series'
    if (nrow(lb) == nrow(series)) {
      series$event <- lb$event
      # Remove time column if present as the first column
      series[1] <- NULL
      out_file <- file.path("etl", "numenta", "source", grp, basename(files_zip[i]))
      dir.create(dirname(out_file), recursive = TRUE, showWarnings = FALSE)
      save(series, file = out_file, compress = TRUE)
    } else {
      message(sprintf("Skipping due to size mismatch: %s vs %s", basename(files_zip[i]), basename(files_lb[i])))
    }
  }
}

# 3) Build grouped datasets as lists by group name
create_grouped <- function(groups, names_vec) {
  dataset <- list()
  for (k in seq_along(groups)) {
    files_sr <- list.files(path = file.path("etl", "numenta", "source", groups[k]), pattern = "\\.RData$")
    group <- list()
    for (i in seq_along(files_sr)) {
      series_file <- file.path("etl", "numenta", "source", groups[k], files_sr[i])
      load(series_file) # loads 'series'
      group[[i]] <- series
    }
    names(group) <- str_sub(files_sr, end = -9)
    dataset[[k]] <- group
  }
  names(dataset) <- names_vec
  dataset
}

# Save grouped objects per group
for (j in seq_along(series_path)) {
  numenta_grp <- create_grouped(series_path[j], series_description[j])
  out_file <- file.path("etl", "numenta", "intermediate", "grouped", paste0("numenta_", series_description[j], ".RData"))
  save(numenta_grp, file = out_file, compress = TRUE)
}

# Save all labeled groups combined (where labels exist)
numenta_grp_all <- create_grouped(lab_path, lab_path)
dir.create(file.path("etl", "numenta", "intermediate", "grouped"), recursive = TRUE, showWarnings = FALSE)
save(numenta_grp_all, file = file.path("etl", "numenta", "intermediate", "grouped", "numenta_grp_all.RData"), compress = TRUE)
