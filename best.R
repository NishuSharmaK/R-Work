best <- function(state, outcome) 
{
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia")
  {
    stop("invalid outcome")
  }
  if (sum(file$State == state) == 0) stop("invalid state")
    
  filest <- subset(file,file$State == state)
  sortedL <- NULL
  if (identical(outcome,"heart attack"))
  {
    ft <- filest[!is.na(filest[[11]]),]
    min <- min(as.numeric(ft[[11]]))
    hospName <- sort(ft[as.numeric(ft[[11]]) == min,][[2]])
  }
  else if(identical(outcome,"heart failure"))
  {
    ft <- filest[!is.na(filest[[17]]),]
    min <- min(as.numeric(ft[[17]]))
    hospName <- sort(ft[as.numeric(ft[[17]]) == min,][[2]])
  }
  else if(identical(outcome,"pneumonia"))
  {
    ft <- filest[!is.na(filest[[25]]),]
    min <- min(as.numeric(ft[[25]]))
    hospName <- sort(ft[as.numeric(ft[[25]]) == min,][[2]])
  }
  hospName[1]
 
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}