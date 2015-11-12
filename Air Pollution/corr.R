setwd("~/GitHub/R/Air Pollution")
corr <- function (directory, threshold = 0){
  
  files <- c( paste(directory, "/", sprintf("%03d",1:332), ".csv", sep="") )
  mydata <- lapply(files, read.csv)
  
  #use complete function to get number of complete entries foreach monitor
  comp <- complete("specdata")
  
  #select rows where completed is over threshold
  over_thre <- subset(comp, select = id , subset = comp[["nobs"]]>threshold)
  
  if (nrow(over_thre)<1) { return } 
  
  final <- sapply( over_thre[[1]], function(x) { data<-as.numeric(cor(mydata[[x]]["sulfate"], mydata[[x]]["nitrate"], use="na.or.complete")) } )
  
  final
}




