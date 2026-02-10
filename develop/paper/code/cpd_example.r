"""
#' @title CPD Example — 3W Oil Wells (Paper Figure)
#' @description Example used to render figures for CPD on the 3W dataset. Shows
#'     a simple binary segmentation pipeline and result plotting.
#' @references Truong, C., Oudre, L., & Vayatis, N. (2020). Selective review of
#'     change point detection methods. Signal Processing, 167, 107299.
"""

library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(tspredit)
library(ggplot2)


# Detection ----------------------------------------
data(oil_3w_Type_1)
data <- oil_3w_Type_1$`WELL-00001_20140124213136`
#series <- data$p_tpt

# Train and detect
model <- hcp_binseg()
model <- fit(model, data$p_tpt)
detection <- detect(model, data$p_tpt)

# Record results (optional)
#md_file <- "develop/cpd_example_model.RData"
#save(model, file = md_file, compress = "xz")

#out <- "develop/cpd_example_det.RData"
#save(detection, file = out, compress = "xz")

#oil_3w_example_series <- "develop/cpd_example_series.RData"
#save(data, file = oil_3w_example_series, compress = "xz")


# Load series and detection ----------------------------------------
#load("develop/cpd_example_series.RData")
#load("develop/cpd_example_model.RData")
#load("develop/cpd_example_det.RData")


# Plot ----------------------------------------
font <- theme(text = element_text(size=16))

grf <- har_plot(model, data$p_tpt, detection,
                data$event, pointsize = 0.15) + font
plot(grf)
