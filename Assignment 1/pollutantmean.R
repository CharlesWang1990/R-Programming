pollutantmean <- function(directory, pollutant, id=1:332) {
 
  #Include leading zeros in the id numbers
  for (i in id){
    if(i <10)  id[match(i,id)] <- paste0('00',i)
    if(i >=10 & i<100) id[match(i,id)] <- paste0('0',i)
  }
  
  #load the data files
  dir <- paste0(getwd(),"/",directory)
  list <- lapply(id,function(x){read.csv(paste0(dir,'/',x,'.csv'))})
  
  #Extract the pollutant variable from all the data files in the list created above
  plist <- lapply(list,function(x){x[,pollutant]})
  
  #Convert the list obtained above to a vector airthmetic mean function can be applied
  pvec <- unlist(plist)
  
  #Compute the mean
  mean(pvec,na.rm=T)  
  
}
