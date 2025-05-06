# dal-events

## Description and Content

Repository for organizing datasets used for developing and testing methods for time series events detection. The repository will preferably consist of datasets with labels to assist in evaluating the performance of the methods applied in the experiments. Although it is possible to use it through files, it is recommended to follow the guidance in the next section for use through this R package.


| Dataset | Labels  | Number of series | Content | Files and Code / Documentation | Origin URL |
| ------- | ------- | ---------------- | ------- | ------------------- | ------ |
| Gecco Challenge | Yes | 9 | Data collection for water quality monitoring | [Gecco Example](https://github.com/cefet-rj-dal/dalevents/blob/main/examples/gecco_samples.md) | [Gecco Challenge 2018](https://www.spotseven.de/gecco/gecco-challenge) |
| Numenta Anomaly Benchmark | Yes | 58 | Cloud services and synthetic data | [Numenta Example](https://github.com/cefet-rj-dal/dalevents/blob/main/examples/nab_samples.md) | [Numenta](https://www.numenta.com) |
| UCR Anomaly Archive | Yes | 250 | Real data from human medicine, biology, meteorology and industry | [UCR Example](https://github.com/cefet-rj-dal/dalevents/blob/main/examples/ucr_samples.md) | [UCR Archive](https://paperswithcode.com/dataset/ucr-anomaly-archive) |
| UCI 3W | Yes | 649 | Oil well drilling | [3W Example](https://github.com/cefet-rj-dal/dalevents/blob/main/examples/3w_samples.md) | [UCI 3W](https://archive.ics.uci.edu/ml/datasets/3W+dataset) |
| RARE | Yes | 7062 | Memory consumption in cloud services | **Done** / In Progress |[RARE](https://doi.org/10.1145/3416505.3423560) |
| Finance Indexes | Yes | 10 | Finance: Securities Price Indexes (Brazil) | **Done** / In Progress | [b3](https://www.b3.com.br/pt_br/market-data-e-indices/) and [BR Investing](https://br.investing.com/indices/) |
| Mortality CNES | No | 1 | Child mortality dataset. Data for assessing infant mortality, an important health indicator. | **Done** / In Progress | |
| EIA Oil Prices | No | 7 | Finance: Prices of crude oil and petroleum products | **Done** / *To Do* | [EIA](https://www.eia.gov/petroleum/data.php) |


## Usage Example

The following usage example describes installing the package `dalevents` and loading a series. This is the easiest and most recommended way to use the series contained in this repository.

```
#Install package
install.packages("devtools")
library(devtools)
devtools::install_github("cefet-rj-dal/dalevents", force=TRUE)
library(dalevents)

#Load a series
data(eia_oil_prices)
series <- eia_oil_prices$Crude_Oil$EER_EPMRU_PF4_Y35NY_DPG

#Use the loaded series
summary(series)
plot(series, type = "l")
```

### Series available in the package

List of series already available for loading through the package. For each series the `data(serie_name)` function must be used, according to the usage example above. This will create a linked list with the same name as the dataset. When browsing through the list the last level will represent each series ready for use with the desired function.


| Dataset Identifier | Loading |
| ---------------- | ---------------- |
| gecco | `data(gecco)` |
| numenta_artificialNoAnomaly | `data(numenta_artificialNoAnomaly)` |
| numenta_artificialWithAnomaly | `data(numenta_artificialWithAnomaly)` |
| numenta_realAdExchange | `data(numenta_realAdExchange)` |
| numenta_realAWSCloudwatch | `data(numenta_realAWSCloudwatch)` |
| numenta_realKnownCause | `data(numenta_realKnownCause)` |
| numenta_realTraffic | `data(numenta_realTraffic)` |
| numenta_realTweets | `data(numenta_realTweets)` |
| rare | `data(rare)` |
| ucr | `data(ucr)` |
| oil_3w_Type_0 | `data(oil_3w_Type_0)` |
| oil_3w_Type_1 | `data(oil_3w_Type_1)` |
| oil_3w_Type_2 | `data(oil_3w_Type_2)` |
| oil_3w_Type_5 | `data(oil_3w_Type_5)` |
| oil_3w_Type_6 | `data(oil_3w_Type_6)` |
| oil_3w_Type_7 | `data(oil_3w_Type_7)` |
| oil_3w_Type_8 | `data(oil_3w_Type_8)` |
| mortality_cnes | `data(mortality_cnes)` |
| fi_br | `data(fi_br)` |
| eia_oil_prices | `data(eia_oil_prices)` |

### Complete original files and ETL process


Another way to use series from this data repository is through files available in https://github.com/cefet-rj-dal/dal-event-etl. Each dataset is organized in a separate directory with documentation about its origin, data files (when available) and R Script for its load. To use the series from the files, the repository must be cloned and the usage examples reproduced from the `original_datasets` directory.
