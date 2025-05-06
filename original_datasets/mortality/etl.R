library(readr)
library(dplyr)
library(reshape)
mortality_and_nasc_by_month_v4 <- read_csv("mortality_and_nasc_by_month_v4.csv", 
                                           col_types = cols(...1 = col_integer(), 
                                                            LOCAL_CODE = col_integer(), grupo = col_integer(), 
                                                            year_month_date = col_date(format = "%Y-%m-%d"), 
                                                            all_death = col_integer(), FL_BIRTH = col_integer(), 
                                                            code = col_integer()))


colnames(mortality_and_nasc_by_month_v4)[c(1,2,3,4,7)] <- c("id", "local_name", "ibge", "group", "fl_birth")
mortality_and_nasc_by_month_v4$region[mortality_and_nasc_by_month_v4$region=="CentroOeste"] <- "Centro-Oeste"
mortality_and_nasc_by_month_v4$region <- as.factor(mortality_and_nasc_by_month_v4$region)
save(mortality_and_nasc_by_month_v4, file="mortality_and_nasc_by_month_v4.RData", compress = TRUE)

load("C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality/data/mortality_and_nasc_by_month_v4.RData")

summary <- mortality_and_nasc_by_month_v4 |> group_by(region, uf, ibge, local_name) |> summarise(group = max(group)) |> arrange(desc(group), region, uf, local_name)
save(summary, file="C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality/data/summary.RData", compress = TRUE)

all_death <- mortality_and_nasc_by_month_v4 |> select(ibge, month = year_month_date, value = all_death) 
all_death <- cast(all_death, month ~ ibge, fun.aggregate = sum)
all_death <- as.list(all_death)
month <- all_death$month
all_death <- all_death[2:length(all_death)]
for (i in 1:length(all_death)) {
  s <- all_death[[i]]
  data <- data.frame(serie = s, event = rep(NA, length(s)))
  rownames(data) <- month
  all_death[[i]] <- data
  
}
save(all_death, file="C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality/data/all_death.RData", compress = TRUE)

fl_birth <- mortality_and_nasc_by_month_v4 |> select(ibge, month = year_month_date, value = fl_birth) 
fl_birth <- cast(fl_birth, month ~ ibge, fun.aggregate = sum)
month <- fl_birth$month
fl_birth <- fl_birth[2:length(fl_birth)]
for (i in 1:length(fl_birth)) {
  s <- fl_birth[[i]]
  data <- data.frame(serie = s, event = rep(NA, length(s)))
  rownames(data) <- month
  fl_birth[[i]] <- data
}
save(fl_birth, file="C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality/data/fl_birth.RData", compress = TRUE)


