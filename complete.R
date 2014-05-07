complete <- function(directory, fid = 1:332) {
  
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV fileS
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  totalRows <- data.frame(id=NULL,nobs=NULL)
  fname <- paste(directory,"/",sprintf("%03d",fid),".csv",sep="",collapse=NULL)
  for (i in 1:length(fid)) {
    file <- read.csv(fname[i],skip=0,header=TRUE)
    dataObs <- subset(file,file$sulfate != "NA" & file$nitrate != "NA" & file$Date != "NA" & file$ID != "NA")
    totalRows <- rbind(totalRows, c(fid[i],nrow(dataObs)))
  }
  colnames(totalRows)<-c("id","nobs")
  #print(totalRows)
  return(totalRows)
}