"""
#' @title UCR Anomaly Archive — ECG Example
#' @description Demonstration script for univariate anomaly detection on an ECG
#'     series from the UCR Anomaly Archive. It covers loading data, simple
#'     preprocessing, model training, detection, visualization, and evaluation.
#' @references Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection:
#'     A survey. ACM Computing Surveys, 41(3), 1–58.
#' @examples
#' # Run step‑by‑step to reproduce the experiment.
"""
## UCR Anomaly Archive — univariate anomaly detection example

library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(tspredit)

## Load series ----------------------
data(ucr_ecg)


## Univariate series selection ----------------------
# Inspect a few series and pick one
for (i in 1:length(ucr_ecg)) {
  plot(as.ts(ucr_ecg[[i]]), main = paste("UCR", i))
}

# Select a representative series
series <- ucr_ecg[[3]]
plot(as.ts(series))



#Labels
#IDX = 5400_5600 -> Range defined in dataset documentation
#series$event <- 0
#series$event[5400:5600] <- 1
#names(series) <- c("value", "event")
#plot(as.ts(series))

# Train/test split cutoff
train_cutoff <- 5000


train <- series[1:train_cutoff, ]
plot(as.ts(train))


test <- series[(train_cutoff + 1):nrow(series), ]
plot(as.ts(test))

## Preprocessing ----------------------
preproc <- ts_norm_gminmax()
preproc <- fit(preproc, series$value)
series$value <- transform(preproc, series$value)
head(series)

plot(as.ts(series))

## Event detection experiment ----------------------
# Define a detector (choose one)
# model <- hanr_arima()
# model <- hanr_fbiad()
# model <- hanr_remd()
model <- han_autoencoder(3, 2, autoenc_ed, num_epochs = 1500)

#Fitting the model
model <- fit(model, train$value)


#Making detections
# Detect anomalies on full series or test subset
detection <- detect(model, test$value)


# Results analysis ----------------------
#Filtering detected events
print(detection |> dplyr::filter(event == TRUE))

#Ploting the results
# Plot detections against labels (test subset)
grf <- har_plot(model, test$value, detection, test$event)
plot(grf)

#Evaluating the detection metrics
ev <- evaluate(model, detection$event, test$event)
print(ev$confMatrix)


# Soft evaluation with temporal tolerance
sw <- 200
ev_soft <- evaluate(har_eval_soft(sw = sw), detection$event, test$event)
print(ev_soft$confMatrix)

print(ev_soft$accuracy)
print(ev_soft$F1)

