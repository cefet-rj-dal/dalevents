"""
#' @title Motif Example — MIT‑BIH V2 (Paper Figure)
#' @description Example used to render figures for motif discovery using SAX on
#'     a slice of MIT‑BIH V2 ECG data.
#' @references Lin, J., Keogh, E., Wei, L., & Lonardi, S. (2007). Experiencing
#'     SAX: a novel symbolic representation of time series. Data Mining and
#'     Knowledge Discovery, 15(2), 107–144.
"""

library(united)
library(daltoolbox)
library(daltoolboxdp)
library(harbinger)
library(tspredit)
library(ggplot2)

# Detection ----------------------------------------
data(mit_bih_V2)
data <- mit_bih_V2$r102_V2[207000:209000,]
series <- data$value

# Train and detect
model <- hmo_sax(21, w = 6, qtd = 12)
model <- fit(model, series)
detection <- detect(model, series)

# Record results (optional)
#md_file <- "motif_example_model.RData"
#save(model, file = md_file, compress = "xz")

#out <- "motif_example_det.RData"
#save(detection, file = out, compress = "xz")

#mit_bih_example_series <- "motif_example_series.RData"
#save(data, file = mit_bih_example_series, compress = "xz")


# Load series and detection ----------------------------------------
#load("develop/motif_example_series.RData")
#load("develop/motif_example_model.RData")
#load("develop/motif_example_det.RData")


# Plot ----------------------------------------
font <- theme(text = element_text(size=16))

grf <- har_plot(model, data$value, detection,
                data$event) + font
plot(grf)


