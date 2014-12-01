best <- function(state,outcome){
  options(warn=-1)

  #Load data and extract relevant columns
  data <- read.csv('outcome-of-care-measures.csv',colClasses='character')
  data <- data[,c(2,7,11,17,23)]
  
  #coerce to appropriate classes
  data[,3:5] <- apply(data[,3:5],2,as.numeric)
    
  #Check validity of input
  if(!(state %in% data[,2])) stop('invalid state')
  outcomes <- c('heart failure','pneumonia','heart attack')
  if(!(outcome %in% outcomes)) stop('invalid outcome')
  
  #Return hospital name in the state with the lowest death rate 
  names(data)[c(4,5,3)] <- outcomes
  output <- lapply(outcomes,function(x){if(outcome==x) data[c('Hospital.Name','State',outcome)]})
  output<-output[-which(sapply(output,is.null))]
  output <- output[[1]]
  output <- output[complete.cases(output),]
  output <- subset(output,output['State']==state)
  output <- subset(output,output[outcome]==min(output[outcome]))
  output <- output[with(output,order(Hospital.Name)),]
  return(output[1,'Hospital.Name'])
}
