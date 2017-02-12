fs.svmGreedy <- function(secom, x){
  require(e1071)
  
  l <- length(x) - 40
  for(j in 1:l){
    # Fit svm model
    fit = svm(LABEL ~ ., data=secom, type='C-classification', kernel='linear')
    
    # Obtain feature weights
    w = t(fit$coefs) %*% fit$SV
    w <- abs(w)
    
    secom[names(x)[which.min(w)]] <- NULL
    x[names(x)[which.min(w)]] <- NULL
  }
}