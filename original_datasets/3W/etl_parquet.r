#########################################################
## Dataset UCI 3W - undesirable real events in oil wells
## ETL to use it in time series event detection
#########################################################

series_description <- c("Type_3",
                        "Type_4",
                        "Type_9")

series_path <- c("3","4","9")


# Create the complete dataset organized as a list -------------------------

create_dataset <- function(group = c("3","4","9"), nm = NA) {
  require(stringr)
  require(readr)
  require(arrow)

  series_path <- group
  ungp_path <- "data/ungrouped/"

  #Dataset with groups organized into a list
  dataset <- list()

  #Iteration on series groups
  for (k in 1:length(series_path)) {
    # Group - Series -----------------------------------------------------
    #files_sr <- list.files(path = series_path[k], pattern = "*.RData")
    files_sr <- list.files(path = paste(ungp_path, series_path[k], sep = ""), pattern = "*.parquet")

    #Dataset organized with series into a list
    group <- list()

    #Iteration in the series of each group
    i <- 1
    for (i in 1:length(files_sr)) {
      series_file <- paste(ungp_path, series_path[k], "/", files_sr[i], sep = "")

      #series <- read_csv(series_file)
      #load(series_file)
      series <- read_parquet(series_file)
      group[[i]] <- series

    }
    #Each dataframe is given the name of the series in the list
    names(group) <- str_sub(files_sr, end = -9)
    dataset[[k]] <- group

  }
  #Each group is named after the original documentation
  names(dataset) <- nm

  # Return complete dataset -------------------------------------------------
  return(dataset)
}


# Use example -------------------------------------------------------------
#Series by group
j=1
out_file <- paste("data/grouped/oil_3w_", series_description[j], ".RData", sep = "")
print(out_file)


oil_3w_Type_3 <- create_dataset(group = series_path[j], nm = series_description[j])
save(oil_3w_Type_3, file = out_file, compress = TRUE)

oil_3w_Type_4 <- create_dataset(group = series_path[j], nm = series_description[j])
save(oil_3w_Type_4, file = out_file, compress = TRUE)

oil_3w_Type_9 <- create_dataset(group = series_path[j], nm = series_description[j])
save(oil_3w_Type_9, file = out_file, compress = TRUE)
