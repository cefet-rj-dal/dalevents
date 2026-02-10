---
title: "Numenta Anomaly Benchmark (NAB) — Cloud Services and Synthetic Data"
output: html_notebook
---

Overview

This notebook demonstrates univariate anomaly detection using series from the
Numenta Anomaly Benchmark (NAB). We will load a synthetic series, train a simple
detector, visualize predictions, and compute metrics.

Dataset notes
- Univariate series with labeled anomalies
- Recommended use: univariate event detection
- Source: https://www.numenta.com/

## Load packages and dataset
Load the required packages for data access, modeling, visualization, and evaluation.

``` r
library(united)
library(daltoolbox)
library(harbinger)
```


Univariate use — Example 1

``` r
## Load series ----------------------
data(nab_artificialWithAnomaly)

# Select the first series from the collection
data <- nab_artificialWithAnomaly[[1]]

# Visualize value and label columns
plot(as.ts(data[, 2:3]),
     main = names(nab_artificialWithAnomaly[1]))
```

![plot of chunk unnamed-chunk-2](fig/nab_samples/unnamed-chunk-2-1.png)

## Event detection experiment
Define a baseline detector and generate anomaly predictions.

``` r
# Establish ARIMA-based detector
model <- hanr_arima()
```



``` r
# Fit the model on the value series
model <- fit(model, data$value)
```



``` r
# Produce anomaly detections
detection <- detect(model, data$value)
```


## Results analysis
Inspect detected positives, plot overlays, and compute metrics.

``` r
# Show detected positives only
print(detection |> dplyr::filter(event == TRUE))
```

```
##     idx event    type
## 1   109  TRUE anomaly
## 2   217  TRUE anomaly
## 3   397  TRUE anomaly
## 4   505  TRUE anomaly
## 5   685  TRUE anomaly
## 6   793  TRUE anomaly
## 7   973  TRUE anomaly
## 8  1081  TRUE anomaly
## 9  1261  TRUE anomaly
## 10 1369  TRUE anomaly
## 11 1549  TRUE anomaly
## 12 1657  TRUE anomaly
## 13 1837  TRUE anomaly
## 14 1945  TRUE anomaly
## 15 2125  TRUE anomaly
## 16 2233  TRUE anomaly
## 17 2413  TRUE anomaly
## 18 2521  TRUE anomaly
## 19 2701  TRUE anomaly
## 20 2809  TRUE anomaly
## 21 2881  TRUE anomaly
## 22 3157  TRUE anomaly
## 23 3277  TRUE anomaly
## 24 3385  TRUE anomaly
## 25 3565  TRUE anomaly
## 26 3673  TRUE anomaly
## 27 3853  TRUE anomaly
## 28 3961  TRUE anomaly
```

Visual analysis

``` r
# Plot predictions (blue) over true labels (red bands)
grf <- har_plot(model, data$value, detection, data$event)
plot(grf)
```

![plot of chunk unnamed-chunk-7](fig/nab_samples/unnamed-chunk-7-1.png)

Evaluate metrics

``` r
# Point-wise evaluation (no temporal tolerance)
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)
```

```
##           event      
## detection TRUE  FALSE
## TRUE      1     27   
## FALSE     0     4004
```

## References

- Lavin, A., & Ahmad, S. (2015). Evaluating real-time anomaly detection algorithms — the Numenta Anomaly Benchmark. 2015 IEEE 14th International Conference on Machine Learning and Applications (ICMLA).
