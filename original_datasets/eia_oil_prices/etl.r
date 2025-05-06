########################################################
## Dataset EIA OIL PRICES
## ETL to use it in time series event detection
########################################################

series_description <- c("Crude_Oil",
                        "Conventional_Gasoline",
                        "RBOB_Regular_Gasoline",
                        "No2_Heating_Oil",
                        "Ultra-Low-Sulfur_No2_Diesel_Fuel",
                        "Kerosene-Type_Jet_Fuel",
                        "Propane")


# Zip original data files as .RData ---------------------------------------
zip_dataset <- function(series_des = NA) {
  require(dplyr)
  require(readr)
  require(stringr)
  require(readxl)
  
  series_path <- "eia_oil_prices/data/"
  series_description <- series_des
  
  series_file <- "PET_PRI_SPT_S1_D.xls"
  series_file <- paste(series_path, series_file, sep = "")
  
  for (k in 1:length(series_description)) {
    series <- read_excel(series_file, sheet = k, skip = 1)

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
  series_path <- "eia_oil_prices/data/zip"
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
oil_prices_all <- create_dataset(series_des = series_description)
out_file <- "eia_oil_prices/data/eia_oil_prices.RData"
save(oil_prices_all, file = out_file, compress = TRUE)
