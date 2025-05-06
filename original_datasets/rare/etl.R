##################################################################
## RARE dataset - time series with monitoring of cloud platforms
## ETL to use it in time series event detection
##################################################################
library(dplyr)
library(readr)
library(readxl)
library(stringr)

#orginal file with format problems
#file <- "RARE.csv"
#rare <- read_delim(file, delim = "|", guess_max = min(4000))


#Fixed file in Python Pandas without format problems
file = "rare_load.csv"
rare <- read_csv(file = file)
str(rare)

#Removing useless columns
rare <- subset(rare, select = -c(1, time, instance))
rare <- rename(rare, event = anomaly)

#Saving series and labels in compressed R file
output = "rare.RData"
save(rare, file = output, compress = TRUE)

#Auxiliary file with series names
series <- read_csv(file = "rare_attributes.csv")


#Series with usefull data
#Use rare_attributes.csv to exploit more series examples
#Kafka => Processor Idle Percent (0 to 8)
plot(rare$kafka_network_processor_idlepercent_value_0, type = "l")
plot(rare$kafka_network_processor_idlepercent_value_1, type = "l")
plot(rare$kafka_network_processor_idlepercent_value_7, type = "l")

#Kafka => Other series
plot(rare$kafka_network_processor_idlepercent_value_0, type = "l")
plot(rare$kafka_server_brokertopicmetrics_bytesinpersec_count_2, type = "l")
plot(rare$kafka_server_brokertopicmetrics_bytesinpersec_oneminuterate_2, type = "l")
plot(rare$kafka_server_brokertopicmetrics_bytesrejectedpersec_count_3, type = "l")
