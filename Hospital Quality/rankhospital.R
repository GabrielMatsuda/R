rankhospital <- function(state, outcome, num = "best") {
  setwd("~/GitHub/R/Hospital Quality")  
  
  ## Read outcome data
  mydata <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

  
  ## Check if outcome is valid
  ## Create alias dataframe to search in csv imported data
  alias <- c("heart attack", "heart failure", "pneumonia")
  names <- c("Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack", "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure", "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia")
  aliases <- data.frame(alias, names)
  param <- as.character(subset(aliases$names, aliases$alias==outcome ))
  if (length(param) == 0){
    stop("invalid outcome")   
  }  
  
  ## Check if state is valid
  ## Search for the state in csv imported datade
  x<-which(mydata$State==state)
  if (length(x) == 0){
    stop("invalid state")   
  }    
  y<- suppressWarnings(mydata[x,])
  
  if(num=="worst") { decrs <- TRUE }
  else { decrs <- FALSE }
  
  if(is.character(num)) { num <- 1 }

  indexes <- suppressWarnings( with(y, order(as.numeric(y[,param]), y[,"Hospital.Name"], decreasing = decrs) ) )
  result <- y[indexes, ]
  result[num, "Hospital.Name"] 
  
}


 