#########################################################
## UCR Anomaly Archive - Time Series Anomaly Benchmark
## ETL to use it in time series event detection
#########################################################

series_path = c("data")

# Zip original data files as .RData ---------------------------------------
zip_dataset <- function(sp = NA) {
  require(readr)
  require(stringr)

  series_path <- sp

  for (k in 1:length(series_path)){
    files_sr <- list.files(path = paste(series_path[k], "original/", sep = "/"), pattern = "*.txt")

    i <- 1
    for (i in 1:length(files_sr)) {
      #Read file as a serie
      series_file <- paste(series_path[k], "original", files_sr[i], sep = "/")
      series <- read_csv(series_file, col_names = FALSE)
      nm <- str_sub(series_file, start = 31, end = -5)
      names(series) <- c(nm)


      #Save compressed files as .RDaata
      out_file <- paste(series_path[k], "/zip/", str_sub(series_file, start = 15, end = -5), ".RData", sep = "")
      save(series, file = out_file, compress = TRUE)
    }
  }
}


zip_dataset(sp = series_path)


# Create the complete dataset organized as a list -------------------------
create_dataset <- function() {
  require(dplyr)
  require(readr)

  #Dataset organized into a list
  dataset <- list()
  series_path <- "data/zip"
  files_sr <- list.files(path = series_path, pattern = "*.RData")

  for (k in 1:length(files_sr)) {
    series_file <- paste(series_path, files_sr[k], sep = "/")
    load(series_file)
    dataset[[k]] <- series

  }
  #Each dataframe is given the name of the series in the list
  names(dataset) <- files_sr

  # Return complete dataset
  return(dataset)
}

#Save all series in a compressed file
ucr <- create_dataset()
out_file <- "data/grouped/ucr.RData"
save(ucr, file = out_file, compress = TRUE)

#Usage
series <- ucr[[134]]

# Random series choice -----------------------------------------------------
library(dalevents)
data(ucr)

#To ensure reproducibility, do not repeat the next step
sample_idx_ucr <- sample(2:length(ucr), 20, replace=FALSE)
sample_idx_ucr

#To reuse the sample in experiments, record the indexes and selection
save(sample_idx_ucr, file = "sample_idx_ucr.RData", compress = TRUE)


ucr_sample <- list()
ucr_sample <- ucr[sample_idx_ucr]

save(ucr_sample, file = "ucr_sample.RData", compress = TRUE)

#Use script to create labels based on UCR Anomaly Archive Documents Rules
source("create_labels.R")
sample_labeled <- create_labels(file = "ucr_sample.RData")
save(sample_labeled, file = "sample_labeled.RData", compress = TRUE)
