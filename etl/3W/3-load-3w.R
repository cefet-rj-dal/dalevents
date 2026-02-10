#########################################################
## UCI 3W - Copy curated RData to harbinger/
## - Input: ETL/3W/source/oil_3w_Type_*.RData
## - Output: harbinger/oil_3w_Type_*.RData
## - Notes: legacy ETL steps are documented in README.md
#########################################################

copy_3w_rdata_by_name <- function(filename) {
  src <- file.path("ETL", "3W", "source", filename)
  if (!file.exists(src)) {
    stop("Missing source file: ", src)
  }
  dir.create("harbinger", recursive = TRUE, showWarnings = FALSE)
  dest <- file.path("harbinger", filename)
  ok <- file.copy(src, dest, overwrite = TRUE)
  if (!ok) {
    stop("Failed to copy to: ", dest)
  }
  invisible(dest)
}

copy_all_3w_rdata <- function() {
  files <- list.files(file.path("ETL", "3W", "source"), pattern = "^oil_3w_Type_.*\\.RData$")
  if (length(files) == 0) {
    stop("No oil_3w_Type_*.RData files found in ETL/3W.")
  }
  for (f in files) {
    copy_3w_rdata_by_name(f)
  }
  invisible(files)
}
