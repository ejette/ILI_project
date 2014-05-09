format_fips <- function(data){
  for (i in 1:length(data)){
    if (nchar(data[i]) < 5){
      len = nchar(data[i])
      data[i] = paste(paste(rep('0', 5 - len), collapse =''),data[i],sep='') 
    }
  }
  return(data)
}