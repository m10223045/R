comb1 <- function(x, y){
  
  # # Binning and Split.
  
  # # The product characteristic variables are categorized into 
  # # six class from very low to very high useing dataBinning function.
  # x <- round(x, digits = 6) * 1000000
  binning.x <- dataBinning(x)
  
  # # Split data into training and test dataset.
  split.x <- dataSplit(x, y)
  train <- split.x$train
  testLabel <- split.x$test$y
  test <- subset(split.x$test, select = -y)
  
  # # Split data into training and test dataset.
  binning.x <- dataSplit(binning.x, y)
  binning.train <- binning.x$train
  binning.testLabel <- binning.x$test$y
  binning.test <- subset(binning.x$test, select = -y)
  
  
  # # ChiMerge:Discretization of numeric attributs. (out=c("symb","num"))
  # library(dprep)
  # train.disc <- chiMerge(train, 1:22, alpha = 0.05, out = "num")
  
  
  # # Recursive setting.
  # getOption('expressions')
  # options(expressions = 50000)
  
  ###########################################################################
  # # Rule extruction.
  
  # # ID3
  modelID3 <- ID3(binning.train,'y')
  pID3 <- predict_ID3(binning.test, modelID3)
  
  # # rpart
  modelRPART <- rpart(y~., data=train, method = "class")
  pRPART <- predict(modelRPART, test, type = "class")
  modelRPART.tree <- rpart(y~., data=train)
  plot(modelRPART.tree)
  text(modelRPART.tree)
  
  # # C5.0
  modelC50 <- C5.0(subset(train, select = -y),as.factor(train$y))
  pC50 <- predict(modelC50, test)
  # plot(modelC50)
  
  # # SVM
  modelSVM <- svm(y ~ ., data=train, type='C-classification', kernel='linear')
  pSVM <- predict(modelSVM, test)
  
  # # SVM use all compenet from PAC s_c5.
  allComp <- data.frame(cbind(cqpmData$pca_c5$scores, y))
  allComp <- dataBinning(allComp)
  allComp.split <- dataSplit(allComp, y)
  modelSVM.allComp <- svm(y ~ ., data = allComp.split$train, type='C-classification', kernel='linear')
  pSVM.allComp <- predict(modelSVM.allComp, subset(allComp.split$test), select = -y)
  
  # # SVM use original secom dataset.
  modelSVM.original <- svm(LABEL ~ ., data=secom, type='C-classification', kernel='linear')
  pSVM.original <- predict(modelSVM.original, secom)
  
  ###########################################################################
  # # Meesure of performace.
  
  # table(Actual=testLabel, Fitted=pID3)
  # table(Actual=testLabel, Fitted=pRPART)
  # table(Actual=testLabel, Fitted=pC50)
  # table(Actual=testLabel, Fitted=pSVM)
  # table(Actual=testLabel, Fitted=pSVM.allComp)
  # table(Actual=y, Fitted=pSVM.original)
  print("ID3")
  print(measureROC(binning.testLabel, pID3))
  print("RPART")
  print(measureROC(testLabel, pRPART))
  print("C50")
  print(measureROC(testLabel, pC50))
  print("SVM")
  print(measureROC(testLabel, pSVM))
  print("SVM use all Comp")
  print(measureROC(testLabel, pSVM.allComp))
  print("SVM use origin secom")
  print(measureROC(y, pSVM.original))
}