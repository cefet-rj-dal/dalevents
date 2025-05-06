#########################################################
## Dataset UCI 3W - undesirable real events in oil wells
## ETL to use it in time series event detection
#########################################################

series_description <- c("Type_0",
                        "Type_1",
                        "Type_2",
                        "Type_5",
                        "Type_6",
                        "Type_7",
                        "Type_8")

series_path <- c("0","1","2",
                "5","6","7","8")


# Zip original data files as .RData ---------------------------------------
zip_dataset <- function(sp = NA) {
  require(readr)
  require(stringr)
  
  series_path <- sp
  
  for (k in 1:length(series_path)){
    files_sr <- list.files(path = paste(series_path[k], sep = ""), pattern = "*.csv")
    
    i <- 1
    for (i in 1:length(files_sr)) {
      #Read file as a serie
      series_file <- paste(series_path[k], files_sr[i], sep = "/")
      series <- read_csv(series_file)
      
      #Save compressed files as .RDaata
      out_file <- paste("zip/", str_sub(series_file, end = -5), ".RData", sep = "")
      save(series, file = out_file, compress = TRUE)
    }
  }
}


zip_dataset(sp = series_path)


# Clean, organize and create ungrouped series -----------------------------

create_ungrouped_dataset <- function(group = c("0","1","2","5","6","7","8")) {
  require(stringr)
  require(readr)
  
  series_path <- group
  zip_path <- "data/zip/"
  
  #Iteration on series groups
  for (k in 1:length(series_path)) {
    zip_files <- list.files(path = paste(zip_path, series_path[k], sep = ""), pattern = "*.RData")
    print("Files: ")
    print(length(zip_files))
    
    #Iteration in the series of each group
    i <- 1
    for (i in 1:length(zip_files)) {
      series_file <- paste(zip_path, series_path[k], "/", zip_files[i], sep = "")
      load(series_file)
      
      #Removing date column
      series[1] <- NULL
      
      #Adjusting column names
      #series <- rename(series, event = class)
      names(series) <- str_replace_all(names(series), "-", "_")
      
      out_file <- str_replace(series_file, ".RData", "sr.RData")
      out_file <- str_replace(out_file, "zip", "ungrouped")
      save(series, file = out_file)
      
      load(out_file)
    }
  }
}


create_ungrouped_dataset()


# Create the complete dataset organized as a list -------------------------

create_dataset <- function(group = c("0","1","2","5","6","7","8"), nm = NA) {
  require(stringr)
  require(readr)
  
  series_path <- group
  ungp_path <- "data/ungrouped/"
  
  #Dataset with groups organized into a list
  dataset <- list()
  
  #Iteration on series groups
  for (k in 1:length(series_path)) {
    # Group - Series -----------------------------------------------------
    #files_sr <- list.files(path = series_path[k], pattern = "*.RData")
    files_sr <- list.files(path = paste(ungp_path, series_path[k], sep = ""), pattern = "*.RData")
    
    #Dataset organized with series into a list
    group <- list()
    
    #Iteration in the series of each group
    i <- 1
    for (i in 1:length(files_sr)) {
      series_file <- paste(ungp_path, series_path[k], "/", files_sr[i], sep = "")
      
      #series <- read_csv(series_file)
      load(series_file)
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
  oil_3w_grp <- create_dataset(group = series_path[j], nm = series_description[j])
  out_file <- paste("data/grouped/oil_3w_", series_description[j], ".RData", sep = "")
  save(oil_3w_grp, file = out_file, compress = TRUE)
}

#Complete series
out_file <- "data/grouped/oil_3w_all.RData"
oil_well_3w_all <- create_dataset(group = series_path, nm = series_description)
save(oil_well_3w_all, file = out_file, compress = TRUE)
