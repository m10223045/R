
preProcessing <- function(x){
  
  # Processing missing value
  l <- ncol(x)-1
  for(col in 1:l){
    x[is.na(x[, col]), col] <- mean(x[,col], na.rm = T)
  }
  
  # Remove colum that have same value
  for(col in names(x)){
    temp <- x[col]
    if(length(unique(data.matrix(temp)))==1){
      x[col] <- NULL
    }
  }
  
  return(x)
}








