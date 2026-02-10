---
title: "GECCO Challenge — Water Quality"
output: html_notebook
---

Overview

This notebook uses the GECCO water quality challenge data to illustrate anomaly
detection in multivariate environmental time series. We will:

- Load the dataset and visualize the multivariate stream
- Select a univariate sensor for a focused experiment
- Train a baseline detector and evaluate results

Dataset notes
- Multivariate series with labeled anomalies
- Recommended use: multivariate or univariate event detection
- Source: https://www.spotseven.de/gecco/gecco-challenge

## Load packages and dataset
Load the required packages for data access, modeling, visualization, and evaluation.

``` r
library(united)
library(daltoolbox)
library(harbinger)
```

``` r
## Load series ----------------------
data(gecco)
# Plot selected multivariate columns
plot(as.ts(gecco$multi[, 2:10]))
```

![plot of chunk unnamed-chunk-2](fig/gecco_samples/unnamed-chunk-2-1.png)

GECCO recommended sample: one day with anomalies


``` r
## Univariate series selection ----------------------
series <- gecco$ph

# Subset to a recommended one-day window with anomalies
series <- series[16500:18000, ]
plot(as.ts(series$value))
```

![plot of chunk unnamed-chunk-3](fig/gecco_samples/unnamed-chunk-3-1.png)

## Event detection experiment
Define a baseline detector and generate anomaly predictions.

``` r
# Establish ARIMA-based detector
model <- hanr_arima()
```


``` r
# Fit the model on the value series
model <- fit(model, series$value)
```


``` r
# Produce anomaly detections
detection <- detect(model, series$value)
```

## Results analysis
Inspect positives, plot overlays, and compute metrics.


``` r
# Show detected positives
print(detection |> dplyr::filter(event == TRUE))
```

```
##    idx event    type
## 1   56  TRUE anomaly
## 2   74  TRUE anomaly
## 3  378  TRUE anomaly
## 4  396  TRUE anomaly
## 5  766  TRUE anomaly
## 6  784  TRUE anomaly
## 7 1024  TRUE anomaly
## 8 1042  TRUE anomaly
```

Visual analysis

``` r
# Plot predictions (blue) versus true labels (red bands)
grf <- har_plot(model, series$value, detection, series$event)
plot(grf)
```

![plot of chunk unnamed-chunk-8](fig/gecco_samples/unnamed-chunk-8-1.png)

Evaluate metrics

``` r
# Point-wise evaluation (no temporal tolerance)
ev <- evaluate(model, detection$event, series$event)
print(ev$confMatrix)
```

```
##           event      
## detection TRUE  FALSE
## TRUE      4     4    
## FALSE     68    1425
```

## References

- Chandola, V., Banerjee, A., & Kumar, V. (2009). Anomaly detection: A survey. ACM Computing Surveys, 41(3), 1–58.
