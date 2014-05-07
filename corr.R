corr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  compRows <- complete(directory)
  corVec <- NULL
  loc <- subset(compRows,compRows$nobs>threshold)
  for(i in loc$id){
    fname <- paste(directory,"/",sprintf("%03d",i),".csv",sep="",collapse=NULL)
    file <- read.csv(fname,skip=0,header=TRUE)
    sul <- data.frame(file$nitrate,file$sulfate)
    #corVec <- append(corVec, cor(sul, use="complete.obs")[2])
    corVec <- append(corVec, round(cor(sul, use="complete.obs")[2],4))
  }
  
  if(length(corVec) == 0L) return(0)
  return(corVec)
}