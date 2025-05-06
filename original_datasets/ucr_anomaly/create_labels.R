##==================================================================================
##
# Before using the label maker on the UCR file,
# you must read the full dataset documents.
# The way the UCR Anomaly Archive suggests event detection
# may be different from simply considering labels as real events.
#
#This script only labels the range proposed in the dataset documentation as events.
##
##==================================================================================

create_labels <- function(file = "ucr_sample.RData"){
  #Automatically generates labels from a sample of the UCR Archive dataset
  #The variable with sample must have ucr_sample denomination
  require(stringr)

  load(file)
  ucr_meta <- names(ucr_sample)

  ## Aplly label script to complete sample dataset
  j = 1
  for (j in 1:length(ucr_meta)) {

    print(paste("Series:", ucr_meta[j]))
    print(paste("dataset_number:", str_split(ucr_meta[j], pattern = "_")[[1]][1]))
    print(paste("mnemonic_name:", str_split(ucr_meta[j], pattern = "_")[[1]][4]))
    print(paste("train_index_end:", str_split(ucr_meta[j], pattern = "_")[[1]][5]))

    begin_anomaly <- as.integer(str_split(ucr_meta[j], pattern = "_")[[1]][6])
    end_anomaly <- str_split(ucr_meta[j], pattern = "_")[[1]][7]
    end_anomaly <- as.integer(str_sub(end_anomaly, end = -7))
    print(paste("begin_anomaly:", begin_anomaly, "- end_anomaly:", end_anomaly))

    tryCatch({
      ucr_sample[[j]]$event <- 0
      names(ucr_sample[[j]]) <- c("series", "event")
      print(head(ucr_sample[[j]]))
      print("... ... ...")

      print(ucr_sample[[j]]$event[begin_anomaly:end_anomaly])
      ucr_sample[[j]]$event[begin_anomaly:end_anomaly] <- 1
      print("... ... ...")

      print(sum(ucr_sample[[j]]$event))
    }, error = function(e){
      message(e)
    })
    print("-------------------------------------------")
  }
  return(ucr_sample)
}
