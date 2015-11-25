best <- function(state, outcome) {
  
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
  ## Search for the state in csv imported data
  x<-which(mydata$State==state)
  y<-suppressWarnings(which.min(mydata[,param][x]))  
  if (length(x) == 0){
    stop("invalid state")   
  } 
  
  ## Return hospital name in that state with lowest 30-day death rate
  mydata[x[[y]],"Hospital.Name"]  
}