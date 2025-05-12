---
title: "Gecco Challenge Dataset - Water Quality"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---
Data collection for water quality monitoring

* Multivariate series with labeled anomalies.
* Recommended use: multivariate or univariate event detection

Source: https://www.spotseven.de/gecco/gecco-challenge


## Load series

``` r
library(dalevents)
library(daltoolbox)
library(harbinger)


## Load series ----------------------
data(gecco)
```

Gecco recommended sample: One day with anomalies


``` r
data <- gecco$gecco
data <- data[16500:18000,]
plot(as.ts(data))
```

![plot of chunk unnamed-chunk-2](fig/gecco_samples/unnamed-chunk-2-1.png)



## Univariate series selection
Select the desired variable directly from preprocessed data.


``` r
series <- data$ph
plot(as.ts(series))
```

![plot of chunk unnamed-chunk-3](fig/gecco_samples/unnamed-chunk-3-1.png)

## Event detection experiment

Detection steps

``` r
#Establishing arima method
model <- hanr_arima()
```



``` r
#Fitting the model
s <- Sys.time()
model <- fit(model, series)
t_fit <- Sys.time()-s
```



``` r
#Making detections
s <- Sys.time()
detection <- detect(model, series)
t_det <- Sys.time()-s
```


## Results analysis



``` r
#Filtering detected events
print(detection |> dplyr::filter(event==TRUE))
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
#Ploting the results
grf <- har_plot(model, series, detection, data$event)
plot(grf)
```

![plot of chunk unnamed-chunk-8](fig/gecco_samples/unnamed-chunk-8-1.png)

Evaluate metrics

``` r
#Evaluating the detection metrics
ev <- evaluate(model, detection$event, data$event)
print(ev$confMatrix)
```

```
##           event      
## detection TRUE  FALSE
## TRUE      4     4    
## FALSE     68    1425
```
