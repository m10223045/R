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
ID3 <- make_Weka_classifier("weka/classifiers/trees/Id3") 
## test it out. 
DF2 <- read.arff(system.file("arff", "contact-lenses.arff", package = "RWeka")) 
ID3(`contact-lenses` ~ ., data = DF2) 


#######
shortTrain <- round(train, digits = 5) * 100
shortTrain <- data.frame(as.factor(shortTrain))
ID3(`y` ~ ., data = shortTrain) 


C45 <- make_Weka_classifier("weka/classifiers/trees/C45") 
C45(`contact-lenses` ~ ., data = DF2) 
