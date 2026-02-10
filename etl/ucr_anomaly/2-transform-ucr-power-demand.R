#########################################################
## UCR Anomaly Archive - Build power_demand subset
## - Input: etl/ucr_anomaly/source/ucr.RData
## - Output: etl/ucr_anomaly/source/ucr_power_demand.RData
## - Notes: selects power demand series and saves labeled subset
#########################################################

load("etl/ucr_anomaly/source/ucr.RData")

#Sample by group
power_demand_series_numbers <- c(44,45,46,47,152,153,154,155,210,211,212)

power_demand_names <- c("044_PowerDemand1","045_PowerDemand2","046_PowerDemand3",
                        "047_PowerDemand4","152_PowerDemand1","153_PowerDemand2",
                        "154_PowerDemand3","155_PowerDemand4","210_Italianpowerdemand",
                        "211_Italianpowerdemand","212_Italianpowerdemand")



data <- ucr[power_demand_series_numbers]
names(data) <- power_demand_names

ucr_power_demand <- data
#Record ------------------
file <- "etl/ucr_anomaly/source/ucr_power_demand.RData"
save(ucr_power_demand, file=file, compress = "xz")
