#!/usr/bin/env Rscript

args <- commandArgs(trailingOnly = TRUE)
pkg <- if (length(args) >= 1) args[[1]] else "united"

library(pkg, character.only = TRUE)

data_dir <- system.file("data", package = pkg)
if (data_dir == "") {
  stop("Package data directory not found for: ", pkg)
}

files <- list.files(data_dir, pattern = "\\.RData$", full.names = FALSE)
if (length(files) == 0) {
  stop("No .RData files found in installed package data/.")
}

failures <- list()

for (f in files) {
  name <- sub("\\.RData$", "", f)
  message("Testing: ", name)
  ok <- tryCatch({
    utils::data(list = name, package = pkg, envir = .GlobalEnv)
    obj <- get(name, envir = .GlobalEnv)
    full <- loadfulldata(obj)
    if (is.null(full)) stop("loadfulldata returned NULL")
    TRUE
  }, error = function(e) {
    failures[[name]] <<- conditionMessage(e)
    FALSE
  })
  if (!ok) message("  FAILED: ", failures[[name]])
}

if (length(failures) > 0) {
  message("Failures: ", length(failures))
  quit(status = 1)
} else {
  message("All datasets loaded successfully.")
}
