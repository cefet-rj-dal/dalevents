#########################################################
## MIT-BIH - Build channel datasets (local split files or bundle)
## - Input: split MIT-BIH RData files in ETL/mit-bih/source/
##          (MIT-BIH-<name>.RData). Falls back to full bundle if present.
## - Output: harbinger/mit_bih_MLII.RData, harbinger/mit_bih_V1.RData,
##           data/mit_bih_V2.RData, data/mit_bih_V5.RData
## - Notes: builds per-channel lists with idx/value/event/seq metadata
## - Dataset origin: https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData
#########################################################

source_dir <- file.path("ETL", "mit-bih", "source")
dir.create(source_dir, recursive = TRUE, showWarnings = FALSE)

split_files <- list.files(
  path = source_dir,
  pattern = "^MIT-BIH-.*\\.RData$",
  full.names = TRUE
)

load_dataset <- function() {
  if (length(split_files) > 0) {
    dataset <- list()
    for (f in split_files) {
      env <- new.env(parent = emptyenv())
      load(f, envir = env)
      obj_names <- ls(env)
      if (length(obj_names) != 1) {
        stop("Expected one object in split file: ", f)
      }
      nm <- obj_names[[1]]
      dataset[[nm]] <- get(nm, envir = env)
    }
    names(dataset) <- names(dataset)
    return(dataset)
  }

  local_file <- Sys.getenv(
    "MIT_BIH_DATA_FILE",
    file.path(source_dir, "MIT-BIH-Dataset.RData")
  )
  if (!file.exists(local_file)) {
    stop(
      "Missing MIT-BIH files. Provide split files in:\n  ",
      source_dir,
      "\nOr place bundle at:\n  ",
      local_file,
      "\nOrigin: https://canopus.eic.cefet-rj.br/data/MIT-BIH/MIT-BIH-Dataset.RData"
    )
  }

  load(local_file)
  if (!exists("dataset")) {
    stop("MIT-BIH bundle did not define `dataset` object.")
  }
  dataset
}

dataset <- load_dataset()

# Build channel-specific datasets (MLII, V1, V2, V5)
levels <- c('\'', '!', '"', '(', ')', '*', '/', '?', '@', '[', ']', '^', '`',
            '|', '~', '+', '=', 'A', 'a', 'B', 'D', 'e', 'E', 'F', 'f', 'J',
            'j', 'L', 'N', 'n', 'p', 'Q', 'R', 'r', 'S', 's', 't', 'T', 'u',
            'V', 'x')

# MLII
j <- 1
mit_bih_MLII <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$MLII)) {
    data <- dataset[[i]]$MLII$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_MLII[[j]] <- data
    names(mit_bih_MLII)[j] <- sprintf("%s_MLII", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_MLII, file = "harbinger/mit_bih_MLII.RData", compress = "xz")

# V1
j <- 1
mit_bih_V1 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V1)) {
    data <- dataset[[i]]$V1$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V1[[j]] <- data
    names(mit_bih_V1)[j] <- sprintf("%s_V1", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V1, file = "harbinger/mit_bih_V1.RData", compress = "xz")

# V2
j <- 1
mit_bih_V2 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V2)) {
    data <- dataset[[i]]$V2$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V2[[j]] <- data
    names(mit_bih_V2)[j] <- sprintf("%s_V2", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V2, file = "harbinger/mit_bih_V2.RData", compress = "xz")

# V5
j <- 1
mit_bih_V5 <- list()
for (i in 1:length(dataset)) {
  if (!is.null(dataset[[i]]$V5)) {
    data <- dataset[[i]]$V5$signal
    data <- data[, 1:4]
    colnames(data) <- c("idx", "value", "event", "seq")
    data$seqlen <- 50
    data$seq <- factor(data$seq, levels = levels)
    data$event <- FALSE
    data$event[!is.na(data$seq)] <- TRUE
    mit_bih_V5[[j]] <- data
    names(mit_bih_V5)[j] <- sprintf("%s_V5", names(dataset[i]))
    j <- j + 1
  }
  if (j > 5) break
}
save(mit_bih_V5, file = "harbinger/mit_bih_V5.RData", compress = "xz")

