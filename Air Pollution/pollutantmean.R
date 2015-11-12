setwd("~/GitHub/R/Air Pollution")
pollutantmean <- function(directory, pollutant, id = 1:332) {
  mydata <- do.call(rbind,lapply(paste(directory,"/", sprintf("%03d",id), ".csv", sep=""), read.csv))
  polmean <- mean(mydata[[pollutant]], na.rm = TRUE)
  print(round(polmean,3))
}