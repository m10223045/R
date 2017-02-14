loadSources <- function(){
 
  lib.tools <- c("RColorBrewer", "caTools","stats", "RColorBrewer", "parcor")
  lib.DT <- c("C50","rpart", "rpart.plot")
  lib.SVM<- c("e1071", "penalizedSVM")
  
  # To install librarys if they are not install..
  install.libs(lib.tools)
  install.libs(lib.DT)
  install.libs(lib.SVM)
  
  # R tools lib
  require(RColorBrewer)
  require(caTools)
  require(stats)
  require(parcor)
  
  # Decision tree lib
  require(C50)
  require(rpart)
  require(rpart.plot)
  
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
  mdList <- c("pca_cqpm.R","pca_cqpm.origin.R")
  mdRoot <- "modeling/"
  for(i in mdList) source(paste(mdRoot,i,sep=""))
  
  # Load rule extraction functions.
  reList <- c("ID3.R")
  reRoot <- "ruleExtraction/"
  for(i in reList) source(paste(reRoot,i,sep=""))
}

is.installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])

install.libs <- function(libs){
  for(i in libs){
    if(!is.installed(i)) install.packages(i)
  }
}

#######################################################################
# # NOTE SPACE

