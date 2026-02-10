"""
#' @title RARE — Cloud‑Native Memory Anomalies Example (WIP)
#' @description Exploratory example for the RARE dataset with multivariate
#'     metrics from cloud environments. Demonstrates simple anomaly detection
#'     workflow, timing, and metric reporting. This dataset is under
#'     analysis — schema and best practices may evolve.
#' @references Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection:
#'     A survey. ACM Computing Surveys, 41(3), 1–58.
#' @examples
#' # Run step‑by‑step to reproduce the experiment.
"""
## RARE — exploratory anomaly detection example (work in progress)
library(united)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(rare)

# RARE dataset content analysis ----------------------
plot(as.ts(rare[,1:10]))
plot(as.ts(rare[,11:20])) #Flat
plot(as.ts(rare[,21:30])) #Flat
plot(as.ts(rare[,31:40]))
plot(as.ts(rare[,41:50])) #Flat
plot(as.ts(rare[,51:60])) #Flat
plot(as.ts(rare[,61:70])) #Flat
plot(as.ts(rare[,71:80])) #Flat
plot(as.ts(rare[,81:90]))
plot(as.ts(rare[,91:100]))

plot(as.ts(rare[,2]))
plot(as.ts(rare[,89]))
plot(as.ts(rare[,90]))


# Series selection ----------------------
series <- rare[2]
plot(as.ts(series))

series$event <- rare$event
names(series) <- c("value", "event")


plot(as.ts(series))


## Event detection experiment ----------------------
# Experiments results organization
experiment <- data.frame(method = "hanr_arima",
                         dataset = "RARE",
                         series = "kube_pod_status_ready_0",
                         elapsed_time_fit = 0,
                         elapsed_time_detection = 0,
                         accuracy = 0,
                         precision = 0,
                         recall = 0,
                         F1 = 0)

head(experiment)

# Define ARIMA‑based detector
model <- hanr_arima()

# Fit the model and time the operation (seconds)
t0 <- Sys.time()
model <- fit(model, series$value)
t_fit <- difftime(Sys.time(), t0, units = "secs")

# Make detections and time the operation (seconds)
t1 <- Sys.time()
detection <- detect(model, series$value)
t_det <- difftime(Sys.time(), t1, units = "secs")


# Results analysis ----------------------
# Filtering detected events
print(detection |> dplyr::filter(event == TRUE))

# Plot detections against labels
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)

# Evaluate detection metrics
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)

## Experiment update ----------------------
# Update experiment table
experiment$elapsed_time_fit[1] <- as.numeric(t_fit)
experiment$elapsed_time_detection[1] <- as.numeric(t_det)
# Metrics
experiment$accuracy[1] <- ev$accuracy
experiment$precision[1] <- ev$precision
experiment$recall[1] <- ev$recall
experiment$F1[1] <- ev$F1

print(experiment)
