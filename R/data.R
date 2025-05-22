#' @name gecco
#' @title Gecco Challenge 2018
#' @description Data collection for water quality monitoring.
#'     Data Type:	Water quality. Category:	Environment. Creation Date	2018.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(gecco)
#' @format A list of time series.
#' @keywords datasets
#' @references Genetic and Evolutionary Computation Conference (Gecco), Association for Computing Machinery (ACM)
#' @source \href{https://www.spotseven.de/gecco/gecco-challenge/}{Gecco Challenge 2018}
#' @examples
#' data(gecco)
#' serie <- gecco[[1]]
"gecco"


#' @name A1Benchmark
#' @title A1Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A1. Category:	Real data. Creation Date	2021.
#'     See Yahoo Sandbox Webscope for detailed information of the dataset.
#'     Labels available? Yes
#' @docType data
#' @usage data(A1Benchmark)
#' @format A list of time series.
#' @keywords datasets
#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#' @source \doi{10.1371/journal.pone.0262463}
#' @examples
#' data(A1Benchmark)
#' serie <- A1Benchmark[[1]]
"A1Benchmark"

#' @name A2Benchmark
#' @title A2Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A2. Category:	Synthetic data. Creation Date	2021.
#'     See Yahoo Sandbox Webscope for detailed information of the dataset.
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(A2Benchmark)
#' @format A list of time series.
#' @keywords datasets
#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#' @source \doi{10.1371/journal.pone.0262463}
#' @examples
#' data(A2Benchmark)
#' serie <- A2Benchmark[[1]]
"A2Benchmark"

#' @name A3Benchmark
#' @title A3Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A3. Category:	Synthetic data with outliers. Creation Date	2021.
#'     See Yahoo Sandbox Webscope for detailed information of the dataset.
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(A3Benchmark)
#' @format A list of time series.
#' @keywords datasets
#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#' @source \doi{10.1371/journal.pone.0262463}
#' @examples
#' data(A3Benchmark)
#' serie <- A3Benchmark[[1]]
"A3Benchmark"

#' @name A4Benchmark
#' @title A4Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A3. Category:	Synthetic data with anomalies and change points. Creation Date	2021.
#'     See Yahoo Sandbox Webscope for detailed information of the dataset.
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(A4Benchmark)
#' @format A list of time series.
#' @keywords datasets
#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#' @source \doi{10.1371/journal.pone.0262463}
#' @examples
#' data(A4Benchmark)
#' serie <- A4Benchmark[[1]]
"A4Benchmark"

#' @name oil_3w_Type_1
#' @title Oil wells dataset - Type 1
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Type 0 removed from this version due to file size.
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_1)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_1)
#' serie <- oil_3w_Type_1[[1]]
"oil_3w_Type_1"

#' @name oil_3w_Type_2
#' @title Oil wells dataset - Type 2
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_2)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_2)
#' serie <- oil_3w_Type_2[[1]]
"oil_3w_Type_2"

#' @name oil_3w_Type_5
#' @title Oil wells dataset - Type 5
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_5)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_5)
#' serie <- oil_3w_Type_5[[1]]
"oil_3w_Type_5"

#' @name oil_3w_Type_6
#' @title Oil wells dataset - Type 6
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_6)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_6)
#' serie <- oil_3w_Type_6[[1]]
"oil_3w_Type_6"

#' @name oil_3w_Type_7
#' @title Oil wells dataset - Type 7
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_7)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_7)
#' serie <- oil_3w_Type_7[[1]]
"oil_3w_Type_7"

#' @name oil_3w_Type_8
#' @title Oil wells dataset - Type 8
#' @description
#' Firts realistic dataset with real events in oil well drilling. The data available
#'     in this package consist of time series already analyzed and applied in
#'     research experiments by the DAL group (Data Analytics Lab). The series are
#'     divided into 7 groups (Type_0, Type_1, Type_2, Type_5, Type_6, Type_7 and Type_8).
#'     Creation date: 2019.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(oil_3w_Type_8)
#' @format A list of time series.
#' @keywords datasets
#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#' @examples
#' data(oil_3w_Type_8)
#' serie <- oil_3w_Type_8[[1]]
"oil_3w_Type_8"

#' @name nab_artificialWithAnomaly
#' @title Numenta Anomaly Benchmark (NAB) artificialWithAnomaly
#' @description
#' Data collection with real-world time-series.
#'     Artificial data with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_artificialWithAnomaly)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_artificialWithAnomaly)
#' data <- nab_artificialWithAnomaly[[1]]
#' series <- data$value
"nab_artificialWithAnomaly"

#' @name nab_realAdExchange
#' @title Numenta Anomaly Benchmark (NAB) realAdExchange
#' @description
#' Data collection with real-world time-series.
#'     Real data with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_realAdExchange)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_realAdExchange)
#' data <- nab_realAdExchange[[1]]
#' series <- data$value
"nab_realAdExchange"

#' @name nab_realAWSCloudwatch
#' @title Numenta Anomaly Benchmark (NAB) realAdExchange
#' @description
#' Data collection with real-world time-series.
#'     Real data from AWS Cloud with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_realAWSCloudwatch)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_realAWSCloudwatch)
#' nab_grp <- nab_realAWSCloudwatch[[1]]
#' serie <- nab_grp[[1]]
"nab_realAWSCloudwatch"

#' @name nab_realKnownCause
#' @title Numenta Anomaly Benchmark (NAB) realKnownCause
#' @description
#' Data collection with real-world time-series.
#'     Real data with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_realKnownCause)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_realKnownCause)
#' nab_grp <- nab_realKnownCause[[1]]
#' serie <- nab_grp[[1]]
"nab_realKnownCause"

#' @name nab_realTraffic
#' @title Numenta Anomaly Benchmark (NAB) realTraffic
#' @description
#' Data collection with real-world time-series.
#'     Real data from online data traffic with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_realTraffic)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_realTraffic)
#' nab_grp <- nab_realTraffic[[1]]
#' serie <- nab_grp[[1]]
"nab_realTraffic"

#' @name nab_realTweets
#' @title Numenta Anomaly Benchmark (NAB) realTraffic
#' @description
#' Data collection with real-world time-series.
#'     Real data from Tweets with anomalies
#'     As part of the Numenta Anomaly Benchmark (NAB), this dataset contains
#'     time series with real and synthetic data. The real data comes from network
#'     monitoring and cloud computing. On the other hand, synthetic data simulate
#'     series with or without anomalies.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(nab_realTweets)
#' @format A list of time series.
#' @keywords datasets
#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Anomaly Benchmark (NAB) Dataset}
#' @examples
#' data(nab_realTweets)
#' nab_grp <- nab_realTweets[[1]]
#' serie <- nab_grp[[1]]
"nab_realTweets"

#' @name ucr_ecg
#' @title UCR Anomaly Benchmark - ECG series
#' @description
#' Data collection with real-world time-series.
#'     Real data from ECG data
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(ucr_ecg)
#' @format A list of time series.
#' @keywords datasets
#' @references UCR Anomaly Archive
#' @source \href{https://paperswithcode.com/dataset/ucr-anomaly-archive/}{UCI Anomaly Archive}
#' @examples
#' data(ucr_ecg)
#' data <- ucr_ecg[[1]]
#' series <- data$value
"ucr_ecg"

#' @name ucr_nasa
#' @title UCR Anomaly Benchmark - NASA spacecraft series
#' @description
#' Data collection with real-world time-series.
#'     Real data from NASA Spacecraft Monitoring Data
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(ucr_nasa)
#' @format A list of time series.
#' @keywords datasets
#' @references UCR Anomaly Archive
#' @source \href{https://paperswithcode.com/dataset/ucr-anomaly-archive/}{UCI Anomaly Archive}
#' @examples
#' data(ucr_nasa)
#' data <- ucr_nasa[[1]]
#' series <- data$value
"ucr_nasa"

#' @name ucr_int_bleeding
#' @title UCR Anomaly Benchmark - Internal Bleeding series
#' @description
#' Data collection with real-world time-series.
#'     Real data from Internal Bleeding Monitoring Data
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(ucr_int_bleeding)
#' @format A list of time series.
#' @keywords datasets
#' @references UCR Anomaly Archive
#' @source \href{https://paperswithcode.com/dataset/ucr-anomaly-archive/}{UCI Anomaly Archive}
#' @examples
#' data(ucr_int_bleeding)
#' data <- ucr_int_bleeding[[1]]
#' series <- data$value
"ucr_int_bleeding"

#' @name ucr_power_demand
#' @title UCR Anomaly Benchmark - Italian Power Demand
#' @description
#' Data collection with real-world time-series.
#'     Real data from Italian Power Demand
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#' @docType data
#' @usage data(ucr_power_demand)
#' @format A list of time series.
#' @keywords datasets
#' @references UCR Anomaly Archive
#' @source \href{https://paperswithcode.com/dataset/ucr-anomaly-archive/}{UCI Anomaly Archive}
#' @examples
#' data(ucr_power_demand)
#' data <- ucr_power_demand[[1]]
#' series <- data$value
"ucr_power_demand"
