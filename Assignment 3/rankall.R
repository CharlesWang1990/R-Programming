rankall <- function(outcome, num='best'){
  options(warn=-1)
  #Load data and extract relevant columns
  data <- read.csv('outcome-of-care-measures.csv',colClasses='character')
  data <- data[,c(2,7,11,17,23)]
  
  #Possible outcomes and validate input
  outcomes=c('heart attack','heart failure','pneumonia')
  names(data)[3:5] <- outcomes
  if(!(outcome %in% outcomes)) stop('invalid outcome')
  
  #Coerce to appropriate classes
  data[3:5] <- apply(data[3:5],2,as.numeric)
  
  #Perform calculations
  output <- data[c('Hospital.Name',outcome,'State')]
  
  #Remove NA's
  output <- output[complete.cases(output),]
  
  #Split dataframe by state
  output <- split(output,output$State)
  
  #Rank hospitals within each state
  output <- lapply(output, function(x) {x<-x[order(x[outcome],x$Hospital.Name),];cbind(x,rank=1:dim(x)[1])})
  
  #Return the desired output
  desired <- data.frame(hospital=0, state=0)
  for (i  in output){
    if(num=='best') {num <- 1;sub <- unname(unlist(subset(i,i$rank==num)[c('Hospital.Name','State')]))}
    else if(num=='worst') {num <- max(i$rank); sub <-unname(unlist(subset(i,i$rank==num)[c('Hospital.Name','State')]));num <-'worst'}
    else if(!(num %in% i$rank)) {sub <- c(NA,i[1,'State'])}
    else sub <-unname(unlist(subset(i,i$rank==num)[c('Hospital.Name','State')]))
    desired <- rbind(desired,sub)
    }
   
  return(desired)
}