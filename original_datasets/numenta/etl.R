#########################################################
## Dataset Numenta - Time Series Event Benchmark
## ETL to use it in time series event detection
#########################################################

series_description <- c("artificialNoAnomaly",
                        "artificialWithAnomaly",
                        "realAdExchange",
                        "realAWSCloudwatch",
                        "realKnownCause",
                        "realTraffic",
                        "realTweets")

series_path <- series_description


# Zip original data files as .RData ---------------------------------------
zip_dataset <- function(sp = NA) {
  require(readr)
  require(stringr)
  
  series_path <- sp
  
  for (k in 1:length(series_path)){
    files_sr <- list.files(path = paste("original/", series_path[k], sep = ""), pattern = "*.csv")
    
    i <- 1
    for (i in 1:length(files_sr)) {
      #Read file as a serie
      series_file <- paste("original", series_path[k], files_sr[i], sep = "/")
      series <- read_csv(series_file)
      
      #Save compressed files as .RDaata
      out_file <- paste("zip/", str_sub(series_file, start = 10, end = -5), ".RData", sep = "")
      save(series, file = out_file, compress = TRUE)
    }
  }
}


zip_dataset(sp = series_path)



# Adding labels (if available) --------------------------------------------
lab_path <- c("artificialWithAnomaly",
              "realAdExchange",
              "realAWSCloudwatch",
              "realKnownCause",
              "realTraffic",
              "realTweets")


k <- 6
files_lb <- list.files(path = paste("labels", lab_path[k], sep = "/"), pattern = "*.Rds")
files_zip <- list.files(path = paste("zip", lab_path[k], sep = "/"), pattern = "*.RData")


for (i in 1:length(files_lb)) {
  if (str_sub(files_zip[i], end = -7) == str_sub(files_lb[i], start = 10, end = -5)) {
    #Loading data
    file_lb <- paste("labels", lab_path[k], files_lb[i], sep = "/")
    gabarito <- readRDS(file_lb)
    names(gabarito) <- c("time", "event")
    file_sr <- paste("zip", lab_path[k], files_zip[i], sep = "/")
    load(file_sr)
    
    if (nrow(gabarito) == nrow(series)) {
      print("Same size series and labels")
      #Adding labels
      print(paste("Adding labels to series", file_sr))
      series$event <- gabarito$event
      series[1] <- NULL
      
      
      out_file <- paste("ungrouped", lab_path[k], files_zip[i], sep = "/")
      
      save(series, file = out_file, compress = TRUE)
    } else {
      print("ERROR: Series and labels of different sizes")
      print(paste(str_sub(files_zip[i], end = -7), nrow(series), "rows", str_sub(files_lb[i], start = 10, end = -5), nrow(gabarito), "rows"))
      
    }} else {
      print("ERROR: Different labels and series")
      print(paste(str_sub(files_zip[i], end = -7), str_sub(files_lb[i], start = 10, end = -5)))
    }
}



# Create the complete dataset organized as a list -------------------------

create_dataset <- function(group = c("0","1","2","5","6"), nm = NA) {
  require(stringr)
  require(readr)
  
  series_path <- group
  ungp_path <- "ungrouped/"
  
  #Dataset with groups organized into a list
  dataset <- list()
  
  #Iteration on series groups
  for (k in 1:length(series_path)) {
    # Group - Series -----------------------------------------------------
    files_sr <- list.files(path = paste(ungp_path, series_path[k], sep = ""), pattern = "*.RData")
    
    #Dataset organized with series into a list
    group <- list()
    
    #Iteration in the series of each group
    i <- 1
    for (i in 1:length(files_sr)) {
      series_file <- paste(ungp_path, series_path[k], "/", files_sr[i], sep = "")
      
      #series <- read_csv(series_file)
      load(series_file)
      #Removing date column
      #series[1] <- NULL #Inactivate this line if the data column has already been removed
      
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
for (j in 1:length(series_path)) {
  numenta_grp <- create_dataset(group = series_path[j], nm = series_description[j])
  out_file <- paste("grouped/numenta_", series_description[j], ".RData", sep = "")
  save(numenta_grp, file = out_file, compress = TRUE)
}

#Complete series
out_file <- "grouped/numenta_grp_all.RData"
numenta_grp_all <- create_dataset(group = series_path, nm = series_description)
save(numenta_grp_all, file = out_file, compress = TRUE)


##Updating series with labels
#Series by group
series_path <- lab_path
series_description <- lab_path

for (j in 1:length(series_path)) {
  numenta_grp <- create_dataset(group = series_path[j], nm = series_description[j])
  out_file <- paste("grouped/numenta_", series_description[j], ".RData", sep = "")
  save(numenta_grp, file = out_file, compress = TRUE)
}

#Complete series
out_file <- "grouped/numenta_grp_all.RData"
numenta_grp_all <- create_dataset(group = series_path, nm = series_description)
save(numenta_grp_all, file = out_file, compress = TRUE)
