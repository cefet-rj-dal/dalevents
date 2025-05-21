
<!-- README.md is generated from README.Rmd. Please edit that file -->

# DAL Events

<!-- badges: start -->

![GitHub Repo stars](https://cranlogs.r-pkg.org/badges/dalevents)
<!-- badges: end -->

Repository for organizing datasets used for developing and testing
methods for time series events detection. The repository will preferably
consist of datasets with labels to assist in evaluating the performance
of the methods applied in the experiments. Although it is possible to
use it through files, it is recommended to follow the guidance in the
next section for use through this R package.

Full description available at:
<https://github.com/cefet-rj-dal/dalevents/wiki>

## Installation

The latest version of DAL Toolbox at CRAN is available at:
<https://CRAN.R-project.org/package=dalevents>

You can install the stable version of DAL Toolbox from CRAN with:

``` r
install.packages("dalevents")
```

You can install the development version of DAL Toolbox from GitHub
<https://github.com/cefet-rj-dal/dalevents> with:

``` r
timeout <- options()$timeout
options(timeout=1200)
devtools::install_github("cefet-rj-dal/dalevents", upgrade="never")
options(timeout=timeout)
```

## Examples

Usage: <https://github.com/cefet-rj-dal/dalevents/tree/main/examples/>

The examples are organized according to available datasets.

``` r
library(dalevents)
```

## Bugs and new features request

<https://github.com/cefet-rj-dal/dalevents/issues>
