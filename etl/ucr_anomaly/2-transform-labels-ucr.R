#########################################################
## UCR Anomaly Archive - Label full dataset
## - Input: etl/ucr_anomaly/intermediate/grouped/ucr.RData
## - Output: etl/ucr_anomaly/source/ucr.RData and ucr_label_interval.RData
## - Notes: derives event labels based on UCR documentation
#########################################################

source("etl/ucr_anomaly/2-transform-create-labels.R")
load("etl/ucr_anomaly/intermediate/grouped/ucr.RData")
ucr_sample <- create_labels(ucr)

# Save labeled outputs (non-interactive batch mode)
ucr_label_interval <- ucr
file <- "etl/ucr_anomaly/source/ucr_label_interval.RData"
save(ucr_label_interval, file = file, compress = "xz")

ucr <- ucr_sample
file <- "etl/ucr_anomaly/source/ucr.RData"
save(ucr, file = file, compress = "xz")

# Optional diagnostics removed for batch ETL.





