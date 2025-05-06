# #Gecco ------------------------------------------------------------------
meta_gecco <- data.frame(
  series = names(gecco_sample)[1:9]
)

meta_gecco$rows <- nrow(gecco_sample)
meta_gecco$events <- sum(gecco_sample$event)

meta_gecco
summary(meta_gecco)


# #YAHOO ------------------------------------------------------------------
meta_yahoo <- data.frame(
  series = names(yahoo_sample)
)

nr <- c()
j = 1
for (j in 1:length(yahoo_sample)){
  #print(nrow(yahoo_sample[[j]]))
  nr <- append(nr, nrow(yahoo_sample[[j]]))
}

meta_yahoo$rows <- nr


ev <- c()
j = 1
for(j in 1:length(yahoo_sample)){
  #print(sum(yahoo_sample[[j]]$event))
  ev <- append(ev, sum(yahoo_sample[[j]]$event))
}

meta_yahoo$events <- ev

meta_yahoo
summary(meta_yahoo)


# #NAB --------------------------------------------------------------------
meta_nab <- data.frame(
  series = names(nab_sample)
)

nr <- c()
j = 1
for (j in 1:length(nab_sample)){
  #print(nrow(nab_sample[[j]]))
  nr <- append(nr, nrow(nab_sample[[j]]))
}

meta_nab$rows <- nr

ev <- c()
j = 1
for(j in 1:length(nab_sample)){
  #print(sum(nab_sample[[j]]$event))
  ev <- append(ev, sum(nab_sample[[j]]$event))
}

meta_nab$events <- ev

meta_nab
summary(meta_nab)


# #RARE -------------------------------------------------------------------
meta_rare <- data.frame(
  series = names(rare_sample)[1:20]
)

meta_rare$rows <- nrow(rare_sample)
meta_rare$events <- sum(rare_sample$event)

meta_rare
summary(meta_rare)


# #UCR --------------------------------------------------------------------
meta_ucr <- data.frame(
  series = names(ucr_sample)
)

nr <- c()
j = 1
for (j in 1:length(ucr_sample)){
  #print(nrow(ucr_sample[[j]]))
  nr <- append(nr, nrow(ucr_sample[[j]]))
}

meta_ucr$rows <- nr


ev <- c()
j = 1
for(j in 1:length(ucr_sample)){
  #print(sum(ucr_sample[[j]]$event))
  ev <- append(ev, sum(ucr_sample[[j]]$event))
}

meta_ucr$events <- ev

meta_ucr
summary(meta_ucr)


# #3W ---------------------------------------------------------------------
meta_3w <- data.frame(
  series = names(oil_3w_sample)
)

nr <- c()
j = 1
for (j in 1:length(oil_3w_sample)){
  #print(nrow(oil_3w_sample[[j]]))
  nr <- append(nr, nrow(oil_3w_sample[[j]]))
}

meta_3w$rows <- nr


ev <- c()
j = 1
for(j in 1:length(oil_3w_sample)){
  #print(sum(oil_3w_sample[[j]]$event))
  ev <- append(ev, sum(oil_3w_sample[[j]]$event))
}

meta_3w$events <- ev

meta_3w
summary(meta_3w)

#Séries com eventos
meta_3w[meta_3w$events > 0,]
summary(meta_3w[meta_3w$events > 0,])

#Exceto séries inválidas (sem observações)
meta_3w[meta_3w$rows > 0,]
summary(meta_3w[meta_3w$rows > 0,])
