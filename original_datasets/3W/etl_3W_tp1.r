data(oil_3w_Type_1)

#New pattern adjust -----------------------
data <- oil_3w_Type_1$Type_1


#Remove old classes -----------------------
for (i in 1:length(data)){
  data[[i]]$class <- NULL
}


#Adjust variables name -----------------------

#names(data[[i]])
#names(data[[1]])[8] <- "event"

for (i in 1:length(data)){
  names(data[[i]])[8] <- "event"
  names(data[[i]]) <- tolower(names(data[[i]]))
}


#Transform boolean to integer to standardize column events with other datasets
for (i in 1:length(data)){
  data[[i]]$event <- as.logical(as.integer(data[[i]]$event))
}


#Plot -----------------------
for (i in 1:length(data)){
  plot(as.ts(data[[i]]),
       main=names(data[i]))
}

#Dataset organized with series into a list
oil_3w_Type_1 <- list()

#Idx variable
for (i in 1:(length(data))) {
  idx <- 1:nrow(data[[i]])
  oil_3w_Type_1[[i]] <- cbind(data.frame(idx), data[[i]])
  names(oil_3w_Type_1)[i] <- names(data)[i]
}


for (i in 1:length(oil_3w_Type_1)){
  oil_3w_Type_1[[i]]$type <- ""
  oil_3w_Type_1[[i]]$type[oil_3w_Type_1[[i]]$event] <- "Change Point"
}


#Plot -----------------------
for (i in 1:length(oil_3w_Type_1)){
  plot(as.ts(oil_3w_Type_1[[i]]),
       main = names(oil_3w_Type_1[i]))
}

#Record
#save(oil_3w_Type_1, file="data/oil_3w_Type_1.RData", compress = "xz")
