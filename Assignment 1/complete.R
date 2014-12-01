complete <- function(directory, id = 1:332) {
  
  #Include leading zeros in the id's
  for (i in id){
    if(i <10)  id[match(i,id)] <- paste0('00',i)
    if(i >=10 & i<100) id[match(i,id)] <- paste0('0',i)
  }
  
  #Load the data files
  dir <- paste0(getwd(),'/',directory)
  list <- lapply(id, function(x) {read.csv(paste0(dir,'/',x,'.csv'))})
  names(list) <- id
  
  #Define an empty dataframe to store the output in
  output <- data.frame(matrix(NA,nrow=length(id),ncol=2,dimnames=list(NULL,c('id','nobs'))))
  
  #Find the number of complete cases and store in output variable
  for (i in 1:length(list)) {
    output[i,'id']<- as.numeric(names(list))[i]
    output[i,'nobs']<- dim(list[[i]][complete.cases(list[[i]]),])[1] 
  }
  return(output)
}
