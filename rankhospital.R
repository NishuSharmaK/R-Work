rankhospital <- function(state, outcome, num = "best") {
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
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  
  if (identical(outcome,"heart attack"))
    row = 11
  else if (identical(outcome,"heart failure"))
    row = 17
  else if (identical(outcome,"pneumonia"))
    row = 25
  
  ft <- filest[!is.na(filest[[row]]),]
  ft[row] <- sapply(ft[row], as.numeric)
  sortedL <- ft[order(ft[,row],ft[,2]),][2]

  
  if(identical(num,"best"))
   head(ft[order(ft[,row],ft[,2]),][2],1L)[[1]]
  else if(identical(num,"worst"))
    tail(ft[order(ft[,row],ft[,2]),][2],1L)[[1]]
  else 
    ft[order(ft[,row],ft[,2]),][2][as.integer(num),]
}