library(dalevents)


## 3W Type 1 - Clean and labels adjustment -------------------
data(oil_3w_Type_1)

#Series 1 ------------------------
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$class))

#Labels adjust
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$class)
names(labels) <- "class"
plot(as.ts(labels$class))

labels$cpd <- 0
head(labels)

#Initial CP
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
i = cp+1
cp = FALSE
print(i)


for (i in 1018:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)

plot(as.ts(labels$cpd),
     main = "Change Points")

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`)
oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`$ T_JUS_CKGL <- NULL

summary(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`)
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00001_20140124213136`))

#Series 2 ------------------------
plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$class))


#Labels adjust
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$class)
names(labels) <- "class"
plot(as.ts(labels$class))

labels$cpd <- 0
head(labels)

#Initial CP
i = 1
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)

plot(as.ts(labels$cpd),
     main = "Change Points")

plot(as.ts(labels))


oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$cpd <- labels$cpd

#Clean NA columns
summary(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`)
oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00002_20140126200050`))

#Series 3 -------------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$class)
names(labels) <- "class"
labels$cpd <- 0
plot(as.ts(labels))


#Initial CP
i = 1
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`)
oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20170801063614`))


#Series 4 ------------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$class)
names(labels) <- "class"
labels$cpd <- 0


#Initial CP
i = 1
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)

plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`)
oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20170802123000`))

#Series 5 -----------------------
labels <- as.data.frame(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$class)
names(labels) <- "class"
labels$cpd <- 0


#Initial CP
i = 1
cp = FALSE
for (i in 1:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 0){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Final CP
start = cp+1
i = cp+1
cp = FALSE
print(i)


for (i in start:nrow(labels)){
  if (cp == FALSE){
    if(!is.na(labels$class[i]) && labels$class[i] != 101){
      print("Ponto de mudança rotulado em:")
      print(i)
      labels$cpd[i] <- 1
      cp = i
    }
  }
}

#Count of CP
sum(labels$cpd)
plot(as.ts(labels))

oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$cpd <- labels$cpd

summary(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`)
oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`$T_JUS_CKGL <- NULL

plot(as.ts(oil_3w_Type_1$Type_1$`WELL-00006_20180618060245`))


#Final check
i = 1
for (i in 1:length(oil_3w_Type_1$Type_1)){
  plot(as.ts(oil_3w_Type_1$Type_1[[i]]),
       main=paste("Series", i))
}


#Record
out_3w <- "data/oil_3w_Type_1.RData"
#save(list = obj_name, file = file, compress = "xz")
save(oil_3w_Type_1, file = out_3w, compress = "xz")




#NAB
for (i in 1:length(numenta_realAdExchange$realAdExchange)){
  plot(as.ts(numenta_realAdExchange$realAdExchange[[i]]))
}

for (i in 1:length(nab_realAdExchange)){
  plot(as.ts(nab_realAdExchange[[i]]))
}
data(nab_artificialWithAnomaly)


data(nab_realAWSCloudwatch)

for (i in 1:length(nab_realAWSCloudwatch)){
  plot(as.ts(nab_realAWSCloudwatch[[i]]))
}
