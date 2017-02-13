# The function can't using in our data. 
# Error in weka.core.UnsupportedAttributeTypeException: weka.classifiers.trees.Id3: Cannot handle string class!
wekaID3 <- function(){
  library(rJava)
  library(RWeka)
  library(RWekajars)
  
  ## look for a package providing id3 
  WPM("refresh-cache") 
  WPM("list-packages", "available") ## look for id3 
  ## install package providing id3 
  WPM("install-package", "simpleEducationalLearningSchemes") 
  ## load the package 
  WPM("load-package", "simpleEducationalLearningSchemes") 
  ## make classifier 
  wekaID3 <- make_Weka_classifier("weka/classifiers/trees/Id3") 
  
  ## test it out. 
  # DF2 <- read.arff(system.file("arff", "contact-lenses.arff", package = "RWeka"))
  # wekaID3(`contact-lenses` ~ ., data = DF2)
  
  testTrain <- train
  testTrain$y[testTrain$y==1] <- 'T'
  testTrain$y[testTrain$y==-1] <- 'F'
  print(testTrain)
  wekaID3(y ~ ., data = testTrain) 
}


