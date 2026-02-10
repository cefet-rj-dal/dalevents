"""
#' @title Anomaly Example — UCR ECG (Paper Figure)
#' @description Example used to render figures for anomaly detection on the UCR
#'     ECG dataset using an autoencoder. Shows zoomed plotting.
#' @references Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection:
#'     A survey. ACM Computing Surveys, 41(3), 1–58.
"""

library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(tspredit)
library(ggplot2)


# Detection ----------------------------------------
data(ucr_ecg)
series <- ucr_ecg[[3]]

# Sample
train_cut = 5000
train <- series[1:train_cut,]

# Train and detect
model <- han_autoencoder(3, 2, autoenc_ed, num_epochs = 1500)
model <- fit(model, train$value)
detection <- detect(model, series$value)

# Record results (optional)
#md_file <- "develop/an_example_model.RData"
#save(model, file = md_file, compress = "xz")

#out <- "develop/an_example_det.RData"
#save(detection, file = out, compress = "xz")

#ucr_example_series <- "develop/an_example_series.RData"
#save(series, file = ucr_example_series, compress = "xz")


# Load series and detection ----------------------------------------
#load("develop/an_example_series.RData")
#load("develop/an_example_model.RData")
#load("develop/an_example_det.RData")


# Plot ----------------------------------------
font <- theme(text = element_text(size=16))

# Zoom
zoom=16600:17200 #Interval to plot
grf <- har_plot(obj=model, serie=series$value[zoom],
                detection=detection[zoom,],
                event=series$event[zoom],
                idx=series$idx[zoom]) + font
plot(grf)
