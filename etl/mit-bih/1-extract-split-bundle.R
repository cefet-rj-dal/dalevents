#########################################################
## MIT-BIH - Split bundle into per-record RData files
## - Input: ETL/mit-bih/source/MIT-BIH-Dataset.RData
## - Output: ETL/mit-bih/source/MIT-BIH-<name>.RData (one per list item)
## - Notes: keeps original names; file name uses list name as suffix
## - Dataset origin: https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData
#########################################################

source_dir <- file.path("ETL", "mit-bih", "source")
bundle_file <- file.path(source_dir, "MIT-BIH-Dataset.RData")
if (!file.exists(bundle_file)) {
  stop("Bundle not found at: ", bundle_file)
}

load(bundle_file)

if (exists("dataset")) {
  items <- dataset
} else {
  objs <- ls()
  if (length(objs) != 1) {
    stop("Expected a single object in bundle or `dataset`.")
  }
  items <- get(objs[[1]])
}

if (is.null(names(items))) {
  stop("MIT-BIH dataset list has no names.")
}

for (nm in names(items)) {
  out_file <- file.path(source_dir, paste0("MIT-BIH-", nm, ".RData"))
  obj <- items[[nm]]
  assign(nm, obj)
  save(list = nm, file = out_file, compress = "xz")
  rm(list = nm)
}

message("Split complete: ", length(items), " files.")
