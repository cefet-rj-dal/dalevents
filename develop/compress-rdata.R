"""
#' @title Recompress RData Files in Package Data Folder
#' @description Utility to recompress all `.RData` files under `data/` using
#'     `xz` (or another method). Helps reduce repository size while preserving
#'     object names and contents.
#' @references Gentleman, R., & Ihaka, R. (2000). R: A Language for Data Analysis
#'     and Graphics. Journal of Computational and Graphical Statistics.
#' @examples
#' # Run from project root to recompress package data
#' # source("develop/compress-rdata.R")
"""

data_path <- "data/"

# List all .RData files
rdata_files <- list.files(data_path, pattern = "\\.RData$", full.names = TRUE)

# Iterate over every .RData file
for (file in rdata_files) {
  # Load file contents into memory
  load(file)

  # Infer saved object name from file name
  obj_name <- tools::file_path_sans_ext(basename(file))

  # Resave in place with desired compression
  save(list = obj_name, file = file, compress = "xz")  # ou compress = "bzip2"
}

