##adding TRIScore and components' scores
data$TRIScore<-rowMeans(data[,5:14])
data$OPP<-rowMeans(data[,5:6])
data$INN<-rowMeans(data[,7:9])
data$DIS<-rowMeans(data[,15:16])
data$INS<-rowMeans(data[,17:19])

#data to tibble
data <- as.tibble(data)
