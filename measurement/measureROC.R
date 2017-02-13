measureROC <- function(actual, fitted){
  t <- table(Actual=actual, Fitted = fitted)
  # t <- table(Actual=testLabel, Fitted=pID3)
  tFrame <- data.frame(t)
  
  TP <- NULL
  FP <- NULL
  FN <- NULL
  TN <- NULL
  
  for(i in 1:length(tFrame[,1])){
    if(tFrame$Actual[i] == -1 && tFrame$Fitted[i] == -1) TP <- tFrame$Freq[i]
    if(tFrame$Actual[i] == 1 && tFrame$Fitted[i] == 1) TN <- tFrame$Freq[i]
    if(tFrame$Actual[i] == 1 && tFrame$Fitted[i] == -1) FP <- tFrame$Freq[i]
    if(tFrame$Actual[i] == -1 && tFrame$Fitted[i] == 1) FN <- tFrame$Freq[i]
  }
  
  P <- TP+FN
  N <- FP+TN
  TP.rate <- TP/P
  FP.rate <- FP/N
  TN.rate <- TN/N
  FN.rate <- FN/P
  ACC <- (TP+TN)/(P+N)
  ROC.Rate <- data.frame(cbind(TP.rate, FP.rate, TN.rate, FN.rate, ACC))
  ROC.Rate <- round(ROC.Rate, digits = 2)
  # colnames(ROC.Rate) <- c("TP.rate=TP/P", "FP.rate=FP/N", "TN.rate=TN/N", "FN.rate=FN/P", "ACC=(TP+TN)/(P+N)")
  ROC.Condition <- data.frame(cbind(P,N,TP,FN,FP,TN))
  
  list(Table = t, ROC.Rate = ROC.Rate, ROC.Condition = ROC.Condition)
}

#################################################################
# # The R OOP class test.
# setClass("Measure.ROC",representation(
#   pretictTure = "numeric",
#   pretictFalse = "numeric",
#   actualTrue = "numeric",
#   actualFalse = "numeric"
# ))
# 
# setGeneric("measure","Measure.ROC",
#             function(p, y){
#               
#             }
#            )