rankhospital <- function(state,outcome, num='best'){
  #Hide warnings
  options(warn=-1)
  #Load data and extract relevant columns
  data <- read.csv('outcome-of-care-measures.csv',colClasses='character')
  data <- data [,c(2,7,11,17,23)]
  
  #Define possible outcomes and validate input
  outcomes <- c('heart attack','heart failure','pneumonia')
  if(!(outcome %in% outcomes)) stop('invalid outcome')
  if(!(state %in% unique(data[,'State']))) stop('invalid state')
  
  #Coerce to appropriate classes
  data[,3:5] <- apply(data[3:5],2,as.numeric)
  
  #Rename columns
  names(data)[3:5] <- outcomes
  
  #Perform calculations 
  output <- subset(data,data['State']==state)
  output <- output[,c('Hospital.Name',outcome)]
  
  #remove NA's
  output <- output[complete.cases(output),]
  
  #Order rate first ,and then hospital names
  output <- output[order(output[outcome], output$'Hospital.Name'),]
  
  #Create rank vector
  rank <- 1:dim(output)[1]
  
  #add the rank vector as a new column to the output
  output <- cbind(output,rank)
  
  #validate rank input
  if(num=='worst') {num <- rank[length(rank)] }
  if(num=='best') {num <- 1}
  if(!(num %in% rank)) return('NA')
  
  return(output[output[,'rank']==num,'Hospital.Name'])
  
}