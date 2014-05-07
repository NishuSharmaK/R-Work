rankall <- function(outcome, num = "best") {
  ## Read outcome data
  file <- read.csv("outcome-of-care-measures.csv", colClasses = "character",na.strings = "Not Available")
  
  ## Check that state and outcome are valid
  if(outcome != "heart attack" & outcome != "heart failure" & outcome != "pneumonia")
  {
    stop("invalid outcome")
  }
  # set the column for outcome in data frame.
  
  if (identical(outcome,"heart attack"))
    row = 11
  else if (identical(outcome,"heart failure"))
    row = 17
  else if (identical(outcome,"pneumonia"))
    row = 25

  rankDF <- NULL
  state <- NULL
  hpname <- NULL
  
  for(state in sort(unique(file[[7]])))
  {
      #print(state)
      #print(row)
      filest <- subset(file,file$State == state)
      ft <- filest[!is.na(filest[[row]]),]
      ft[row] <- sapply(ft[row], as.numeric)
      sortedL <- ft[order(ft[,row],ft[,2]),][2]
  
      if(identical(num,"best"))
          hpname <- head(ft[order(ft[,row],ft[,2]),][2],1L)[[1]]
      else if(identical(num,"worst")) {
          #print("calculating worst")
          hpname <- tail(ft[order(ft[,row],ft[,2]),][2],1L)[[1]]
      }
      else 
          hpname <- ft[order(ft[,row],ft[,2]),][2][as.integer(num),]
      
      rankDF <- rbind(rankDF,c(hpname,state))
      
  }
  
  rankDF <- as.data.frame(rankDF)
  colnames(rankDF) <- c("hospital","state")
  rownames(rankDF) <- rankDF[,2]
  rankDF  
}