library(readr)
library(dplyr)
library(reshape)
library(lubridate)

yearmonth <- function(day) {
  return(year(day)*100 + month(day))
}

load("C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality_cnes/data/TMN_CNES.RData")

data_clean <- data

summary <- data_clean |> group_by(CNES, uf_REGIAO, uf_SIGLA_UF, mun_MUNNOME, VINC_SUS) |> 
  summarise(obs = n(), 
            inicio = min(COMPETEN), 
            fim=max(COMPETEN), 
            Nascimentos  = mean(Nascimentos, na.rm=TRUE),
            neo_Obitos = mean(neo_Obitos, na.rm=TRUE),
            neo_pre_Obitos = mean(neo_pre_Obitos, na.rm=TRUE),
            neo_tar_Obitos = mean(neo_tar_Obitos, na.rm=TRUE),
            cred_IHAC = max(cred_IHAC),
            IHAC = max(IHAC, na.rm=TRUE),
            cred_BLH = max(cred_BLH),
            BLH = max(BLH, na.rm=TRUE),
            cred_canguru = max(cred_canguru),
            canguru = max(canguru, na.rm=TRUE),
            VINC_SUS=max(VINC_SUS)
            ) 
dist <- summary(summary$obs)
summary$cred_IHAC <- yearmonth(summary$cred_IHAC)
summary$cred_canguru <- yearmonth(summary$cred_canguru)
summary$cred_BLH <- yearmonth(summary$cred_BLH)


summary <- summary |>  filter(obs > dist[3]) |> arrange(desc(Nascimentos))
#summary <- head(summary, 256)
save(summary, file="C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality_cnes/data/summary.RData", compress = TRUE)

datasets <- list()
for (i in 1:nrow(summary)) {
  data <- data_clean |> filter(CNES == summary$CNES[i]) |> 
    select(COMPETEN, Nascimentos, neo_Obitos, neo_pre_Obitos, neo_tar_Obitos) |>
    arrange(COMPETEN)
  
  data$COMPETEN <- as.integer(data$COMPETEN)
  
  data$eventIHAC <- FALSE
  data$eventBLH <- FALSE
  data$eventcanguru <- FALSE
  if (!is.na(summary$cred_IHAC[i]))
    data$eventIHAC[data$COMPETEN == summary$cred_IHAC[i]] <- TRUE
  if (!is.na(summary$cred_BLH[i]))
    data$eventBLH[data$COMPETEN == summary$cred_BLH[i]] <- TRUE
  if (!is.na(summary$cred_IHAC[i]))
    data$eventcanguru[data$COMPETEN == summary$cred_canguru[i]] <- TRUE
  datasets[[i]] <- data
  names(datasets)[i] <- summary$CNES[i]
}

data <- datasets[[6]]

save(summary, file="C:/Users/eduar/OneDrive/git/dal/event_datasets/mortality_cnes/data/datasets.RData", compress = TRUE)
