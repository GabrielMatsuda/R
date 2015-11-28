rankall <- function(outcome, num = "best") {
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

  ## Reads State column to get all states to read
  states <- unique(mydata$State)
  
  if(num=="worst") { decrs <- TRUE }
  else { decrs <- FALSE }
  
  if(is.character(num)) { num <- 1 }
  
  ## Function to get rank of each state
  rank <- function(state)
  {
    hospitals <- suppressWarnings(mydata[which(mydata$State==state),])
    sortIdx <- suppressWarnings( with(hospitals, order(as.numeric(hospitals[,param]), hospitals[,"Hospital.Name"], decreasing = decrs) ) ) 
    hospitals <- hospitals[sortIdx, ]
    c(hospitals[num, "Hospital.Name"], state)
  }
  
  ## Finally, bind the results for each state
  res <- as.data.frame(do.call("rbind" , lapply(states, rank)))
  names(res) <- c("hospital", "state")

  ## Sort results by state
  sort <- suppressWarnings( with(res, order(res[,"state"]) ) )
  res <- res[sort,]
}


