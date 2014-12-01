corr <- function(directory, threshold = 0) {
  
  #Load the csv files in the directory
  dir <- paste0(getwd(),'/',directory)
  filenames <- list.files(dir,pattern='.csv')
  list <- lapply(filenames, function(x){read.csv(paste0(dir,'/',x))})
  
  #Create an empty vector to store the output into
  output <- numeric()
  
  #Look for cases where the complete cases are greater than threshold and store the output in the vector created above
  for(i in 1:length(list)){
    
    if(threshold<dim(list[[i]][complete.cases(list[[i]]),])[1]) {output<-append(output,cor(list[[i]][,2],list[[i]][,3],use='complete.obs'))}    
  
  }
  return(output)
}