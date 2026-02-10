#########################################################
## ETL Orchestrator - Build all package RData artifacts
## Usage:
##   - source("ETL/etl.R")   (from repository root)
##   - Rscript ETL/etl.R
## Notes:
## - Runs ETL scripts in dependency order.
## - Requires raw/source data to be present in ETL subfolders.
#########################################################

ensure_repo_root <- function() {
  if (!dir.exists("ETL")) {
    stop("Run this script from the repository root (missing ETL/).")
  }
  if (!dir.exists("harbinger")) {
    dir.create("harbinger", recursive = TRUE, showWarnings = FALSE)
  }
}

run_script <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("ETL script not found: %s", path))
  }
  message("==> ", path)
  source(path, local = new.env(parent = globalenv()))
}

ensure_repo_root()

# 3W Oil Wells
source("ETL/3W/3-load-3w.R")
copy_all_3w_rdata()

# GECCO Challenge 2018
run_script("ETL/gecco/3-load-gecco.R")

# MIT-BIH Arrhythmia
mit_bih_source <- file.path("ETL", "mit-bih", "source")
mit_bih_split <- list.files(mit_bih_source, pattern = "^MIT-BIH-.*\\.RData$", full.names = TRUE)
mit_bih_bundle <- file.path(mit_bih_source, "MIT-BIH-Dataset.RData")
if (length(mit_bih_split) == 0 && file.exists(mit_bih_bundle)) {
  run_script("ETL/mit-bih/1-extract-split-bundle.R")
}
run_script("ETL/mit-bih/3-load-mit-bih.R")

# Numenta Anomaly Benchmark (NAB)
run_script("ETL/numenta/1-extract-numenta.R")
run_script("ETL/numenta/3-load-artificialWithAnomaly.R")
run_script("ETL/numenta/3-load-realAdExchange.R")
run_script("ETL/numenta/3-load-realAWSCloudwatch.R")
run_script("ETL/numenta/3-load-realKnownCause.R")
run_script("ETL/numenta/3-load-realTraffic.R")
run_script("ETL/numenta/3-load-realTweets.R")

# UCR Anomaly Archive
run_script("ETL/ucr_anomaly/1-extract-ucr.R")
run_script("ETL/ucr_anomaly/2-transform-labels-ucr.R")
run_script("ETL/ucr_anomaly/2-transform-ucr-ecg.R")
run_script("ETL/ucr_anomaly/2-transform-ucr-internal-bleeding.R")
run_script("ETL/ucr_anomaly/2-transform-ucr-nasa.R")
run_script("ETL/ucr_anomaly/2-transform-ucr-power-demand.R")
run_script("ETL/ucr_anomaly/3-load-ucr-ecg.R")
run_script("ETL/ucr_anomaly/3-load-ucr-internal-bleeding.R")
run_script("ETL/ucr_anomaly/3-load-ucr-nasa.R")
run_script("ETL/ucr_anomaly/3-load-ucr-power-demand.R")

# Yahoo Webscope S5
run_script("ETL/yahoo/3-load-yahoo.R")

message("ETL complete.")



#########################################################
## ETL - Build mini RData files for package data/
## - Input: harbinger/*.RData (full datasets)
## - Output: data/*.RData (mini datasets)
## - Rules:
##   - data.frame: keep first 30 rows
##   - list: keep only first element; if it is data.frame, keep first 30 rows
##   - attr(url) points to full dataset in harbinger on GitHub
#########################################################

harbinger_dir <- "harbinger"
output_dir <- "data"

if (!dir.exists(harbinger_dir)) {
  stop("Missing harbinger/ directory.")
}
dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

make_mini <- function(obj) {
  if (is.data.frame(obj)) {
    return(utils::head(obj, 30))
  }
  if (is.list(obj)) {
    if (length(obj) == 0) return(obj)
    first <- obj[[1]]
    if (is.data.frame(first)) {
      first <- utils::head(first, 30)
    }
    out <- list(first)
    names(out) <- names(obj)[1]
    return(out)
  }
  obj
}

files <- list.files(harbinger_dir, pattern = "\\.RData$", full.names = TRUE)
if (length(files) == 0) {
  stop("No .RData files found in harbinger/.")
}

for (f in files) {
  env <- new.env(parent = emptyenv())
  load(f, envir = env)
  obj_names <- ls(env)
  if (length(obj_names) == 0) next
  obj_names <- obj_names[order(obj_names)]
  name <- obj_names[[1]]
  obj <- get(name, envir = env)
  mini <- make_mini(obj)
  attr(mini, "url") <- sprintf(
    "https://raw.githubusercontent.com/cefet-rj-dal/united/refs/heads/main/harbinger/%s",
    basename(f)
  )
  assign(name, mini)
  save(list = name, file = file.path(output_dir, basename(f)), compress = "xz")
  rm(list = name)
}

