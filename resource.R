
loadSources <- function(){
  library(RColorBrewer)
  
  # Decision tree lib
  require(C50)
  require(caTools)
  require(rpart)
  
  # SVM lib
  require(e1071)
  
  # Load measure function.
  mf <- "measurement/measureROC.R"
  source(mf)
  
  # Load proProcessing function.
  pp <- "preProcessing/preProcessing.R"
  source(pp)
  
  # Load plot function
  source("plot/plotFunctions.R")
  
  # Load feature selection functions.
  fsList <- c("fsRidgeCV.R", "fsSvmGreedy.R", "fsSvm.R")
  fsRoot <- "preProcessing/featureSelection/"
  for(i in fsList) source(paste(fsRoot,i,sep=""))
  
  # Load modeling functions.
  mdList <- c("pca_cqpm.R")
  mdRoot <- "modeling/"
  for(i in mdList) source(paste(mdRoot,i,sep=""))
  
  # Load rule extraction functions.
  reList <- c("ID3.R")
  reRoot <- "ruleExtraction/"
  for(i in reList) source(paste(reRoot,i,sep=""))
}
