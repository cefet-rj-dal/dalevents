#' @name gecco
#' @title Gecco Challenge 2018
#' @description Data collection for water quality monitoring.
#'     Data Type:	Water quality. Category:	Environment. Creation Date	2018.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#' @docType data
#' @usage data(gecco)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references Genetic and Evolutionary Computation Conference (Gecco), Association for Computing Machinery (ACM)
#'
#' @source \href{https://www.spotseven.de/gecco/gecco-challenge}{Gecco Challenge 2018}
#'
#' @examples
#' data(gecco)
#' serie <- gecco[[1]]
"gecco"


#' @name A1Benchmark
#' @title A1Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A1. Category:	Real data. Creation Date	2021.
#'     See \href{https://webscope.sandbox.yahoo.com/catalog.php?datatype=s&did=70}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#' @docType data
#' @usage data(A1Benchmark)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#'
#' @source \href{https://doi.org/10.1371/journal.pone.0262463}{Yahoo dataset, 2021}
#'
#' @examples
#' data(A1Benchmark)
#' serie <- A1Benchmark[[1]]
"A1Benchmark"

#' @name A2Benchmark
#' @title A2Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A2. Category:	Synthetic data. Creation Date	2021.
#'     See \href{https://webscope.sandbox.yahoo.com/catalog.php?datatype=s&did=70}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#' @docType data
#' @usage data(A2Benchmark)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#'
#' @source \href{https://doi.org/10.1371/journal.pone.0262463}{Yahoo dataset, 2021}
#'
#' @examples
#' data(A2Benchmark)
#' serie <- A2Benchmark[[1]]
"A2Benchmark"

#' @name A3Benchmark
#' @title A3Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A3. Category:	Synthetic data with outliers. Creation Date	2021.
#'     See \href{https://webscope.sandbox.yahoo.com/catalog.php?datatype=s&did=70}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#' @docType data
#' @usage data(A3Benchmark)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#'
#' @source \href{https://doi.org/10.1371/journal.pone.0262463}{Yahoo dataset, 2021}
#'
#' @examples
#' data(A3Benchmark)
#' serie <- A3Benchmark[[1]]
"A3Benchmark"

#' @name A4Benchmark
#' @title A4Benchmark
#' @description S5 - A Labeled Anomaly Detection Dataset.
#'     Data Type:	Benchmark A3. Category:	Synthetic data with anomalies and change points. Creation Date	2021.
#'     See \href{https://webscope.sandbox.yahoo.com/catalog.php?datatype=s&did=70}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#' @docType data
#' @usage data(A4Benchmark)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references Yoshihara K, Takahashi K (2022) A simple method for unsupervised anomaly detection: An application to Web time series data. PLoS ONE 17(1).
#'
#' @source \href{https://doi.org/10.1371/journal.pone.0262463}{Yahoo dataset, 2021}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_1)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_2)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_5)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_6)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_7)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
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
#'
#'
#' @docType data
#' @usage data(oil_3w_Type_8)
#' @format A list of time series.
#' @keywords datasets
#'
#'#' @references 3W dataset Data Set
#' @source \href{https://archive.ics.uci.edu/ml/datasets/3W+dataset}{UCI Machine Learning Repository}
#'
#' @examples
#' data(oil_3w_Type_8)
#' serie <- oil_3w_Type_8[[1]]
"oil_3w_Type_8"

#' @name numenta_artificialWithAnomaly
#' @title Numenta Benchmark artificialWithAnomaly
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
#'
#'
#' @docType data
#' @usage data(numenta_artificialWithAnomaly)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_artificialWithAnomaly)
#' numenta_grp <- numenta_artificialWithAnomaly[[1]]
#' serie <- numenta_grp[[1]]
"numenta_artificialWithAnomaly"

#' @name numenta_realAdExchange
#' @title Numenta Benchmark realAdExchange
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
#'
#'
#' @docType data
#' @usage data(numenta_realAdExchange)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_realAdExchange)
#' numenta_grp <- numenta_realAdExchange[[1]]
#' serie <- numenta_grp[[1]]
"numenta_realAdExchange"

#' @name numenta_realAWSCloudwatch
#' @title Numenta Benchmark realAdExchange
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
#'
#'
#' @docType data
#' @usage data(numenta_realAWSCloudwatch)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_realAWSCloudwatch)
#' numenta_grp <- numenta_realAWSCloudwatch[[1]]
#' serie <- numenta_grp[[1]]
"numenta_realAWSCloudwatch"

#' @name numenta_realKnownCause
#' @title Numenta Benchmark realKnownCause
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
#'
#'
#' @docType data
#' @usage data(numenta_realKnownCause)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_realKnownCause)
#' numenta_grp <- numenta_realKnownCause[[1]]
#' serie <- numenta_grp[[1]]
"numenta_realKnownCause"

#' @name numenta_realTraffic
#' @title Numenta Benchmark realTraffic
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
#'
#'
#' @docType data
#' @usage data(numenta_realTraffic)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_realTraffic)
#' numenta_grp <- numenta_realTraffic[[1]]
#' serie <- numenta_grp[[1]]
"numenta_realTraffic"

#' @name numenta_realTweets
#' @title Numenta Benchmark realTraffic
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
#'
#'
#' @docType data
#' @usage data(numenta_realTweets)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references Numenta
#' @source \href{https://github.com/numenta/NAB/tree/master/data}{Numenta Benchmark Dataset}
#'
#' @examples
#' data(numenta_realTweets)
#' numenta_grp <- numenta_realTweets[[1]]
#' serie <- numenta_grp[[1]]
"numenta_realTweets"

#' @name ucr
#' @title UCR Anomaly Archive
#' @description
#' Data with 250 series with real world data and synthetic and real anomalies.
#'     Real data from human medicine, biology, meteorology and industry.
#'     See \href{https://github.com/cefet-rj-dal/dalevents}{cefet-rj-dal/dalevents}
#'     for detailed guidance on using this package and the other datasets available in it.
#'     Labels available? Yes
#'
#'
#' @docType data
#' @usage data(ucr)
#' @format A list of time series.
#' @keywords datasets
#'
#'#'#' @references UCR Anomaly Archive
#' @source \href{https://paperswithcode.com/dataset/ucr-anomaly-archive}{UCR Anomaly Archive}
#'
#' @examples
#' data(ucr)
#' serie <- ucr[[1]]
"ucr"
