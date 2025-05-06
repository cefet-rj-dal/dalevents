########################################################
## Finance Indexes
## ETL to use it in time series event detection
########################################################


series_description <- c("Commodity",
                        "Confidence",
                        "Currency",
                        "Desagreement",
                        "DLSP",
                        "Inflation",
                        "Interest_Rate",
                        "NFSP",
                        "Output",
                        "Stocks")


# Zip original data files as .RData ---------------------------------------
zip_dataset <- function(series_des = NA) {
  require(dplyr)
  require(readr)
  require(stringr)
  require(readxl)
  
  series_path <- "FI_BR/data/"
  series_description <- series_des
  
  series_file <- "fi_consolidada.xlsx"
  series_file <- paste(series_path, series_file, sep = "")
  
  for (k in 1:length(series_description)) {
    series <- read_excel(series_file, sheet = k)
    
    out_file <- paste(series_path, "zip/", series_description[k], ".RData", sep = "")
    
    save(series, file = out_file, compress = TRUE)
    
  }
}

zip_dataset(series_des = series_description)


# Create the complete dataset organized as a list -------------------------
create_dataset <- function(series_des = NA) {
  require(dplyr)
  require(readr)
  
  #Dataset organized into a list
  series_description <- series_des
  dataset <- list()
  series_path <- "FI_BR/data/zip"
  files_sr <- list.files(path = series_path, pattern = "*.RData")
  
  for (k in 1:length(files_sr)) {
    series_file <- paste(series_path, files_sr[k], sep = "/")
    load(series_file)
    series[1] <- NULL
    dataset[[k]] <- series
  }
  #Each dataframe is given the name of the series in the list
  names(dataset) <- series_des
  
  # Return complete dataset
  return(dataset)
}


#Save all series in a compressed file
fi_br <- create_dataset(series_des = series_description)
out_file <- "fi_br/data/fi_br.RData"
save(fi_br, file = out_file, compress = TRUE)
