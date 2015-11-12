setwd("~/GitHub/R/Air Pollution")
complete <- function (directory, id = 1:332){

  files<-c( paste(directory, "/", sprintf("%03d",id), ".csv", sep="") )
  mydata <- lapply(files, read.csv)
  
  x <- NULL
  i <- 1
  
  while(i<=length(id))
  {
    aux <- data.frame(id[[i]], nrow(subset( mydata[i][[1]], subset=!is.na(sulfate)&!is.na(nitrate) )))
    names(aux) <- c("id", "nobs")
    
    x <- rbind(x, aux)
    names(x) <- c("id", "nobs")
    i<-i+1
  }
  x
}