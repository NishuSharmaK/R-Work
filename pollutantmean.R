pollutantmean <- function(directory, pollutant, id = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  fname <- paste(directory,"/",sprintf("%03d",id),".csv",sep="",collapse=NULL)
  filepol <- NULL
  for (i in 1:length(id)) {
    file <- read.csv(fname[i],skip=0,header=TRUE)
    if(pollutant == "sulfate")
      filepol <-c(filepol, file$sulfate)
    else if(pollutant == "nitrate")
      filepol <-c( filepol, file$nitrate)
    else
        print("Not the correct pollutant!")
  }
  myMean <- mean(filepol[!is.na(filepol)])
  print(round(myMean,3))   
}