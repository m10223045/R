# Feature selection ues ridge regression and corss validation.
fs.ridge <- function(x, y, n){
  # library(parcor)
  
  system.time(ridge <- ridge.cv(data.matrix(x),y))
  #ridge <- ridge.cv(data.matrix(x),y)
  fs.coef <- abs(ridge$coefficients)
  fs.temp <- NULL
  
  l <- length(fs.coef)
  w.rank <- rank(fs.coef)
  for(i in names(x)){
    j <- paste("X",i,sep="")
    id <- grep(paste("^",j,"$",sep=""), colnames(data.frame(t(fs.coef))))
    
    if( w.rank[id] <= l-40 ){
      #fs.temp <- cbind(fs.temp,fs.coef[id])
      x[i] <- NULL
    }
  }
  return(x)
}
