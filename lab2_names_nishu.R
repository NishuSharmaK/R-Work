require(stringr)

topfivefemalenames <- function(gender,year)
{
  genderfn<-paste(gender,"_names.csv",sep="",collapse=NULL)
  print(genderfn)
  fn=paste("../datasets/Ontario_names/",genderfn,sep="",collapse=NULL)
  print(fn)
  file <- read.csv(fn,skip=1,header=TRUE)
  head(file)
  year = as.integer(year)
  data = file[file$Year==year,]
  data$Frequency = as.integer(data$Frequency)
  dataSorted = data[order(data$Frequency,decreasing=T),]
  topFive = head(dataSorted,n=5L)
  print(topFive)
  
}
topfivefemalenames("female",2009)