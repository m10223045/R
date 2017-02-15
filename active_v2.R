# #  START  # #
setwd("/Volumes/SANDISK_III/project/R/CQPMpca_id3/qrm")

# # Loading function and library
source("resource.R")
loadSources()

# # Subset the secom dataset to only 2 labels
# # Where –1 corresponds to a pass and 1 corresponds to a fail.
secom <- read.csv("dataset/secom/secom.csv")
secom <- preProcessing(secom)

x <- subset(secom, select = -LABEL)
y <- secom$LABEL

###########################################################################
# # Feature selection.

x <- fs.ridge(x,y,40)
# fs.svm.x <- fs.svm(secom, y)

###########################################################################
# # Modeling.

# cqpmData <- pca.cqpm(x)
cqpmData <- pca.cqpm.origin(secom)
data_temp <- cqpmData$c5
data_temp <- data.frame(cbind(data_temp,y))

###########################################################################
# # Binning and Split.

# # The product characteristic variables are categorized into 
# # six class from very low to very high useing dataBinning function.
# data_temp <- round(data_temp, digits = 6) * 1000000
binning.data_temp <- dataBinning(data_temp)


# # Split data into training and test dataset.
split.data_temp <- dataSplit(data_temp, y)
train <- split.data_temp$train
testLabel <- split.data_temp$test$y
test <- subset(split.data_temp$test, select = -y)

# # Split data into training and test dataset.
binning.data_temp <- dataSplit(binning.data_temp, y)
binning.train <- binning.data_temp$train
binning.testLabel <- binning.data_temp$test$y
binning.test <- subset(binning.data_temp$test, select = -y)


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
plot(modelC50)

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
split.secom <- dataSplit(secom, secom$LABEL)
modelSVM.original <- svm(LABEL ~ ., data=split.secom$train, type='C-classification', kernel='linear')
pSVM.original <- predict(modelSVM.original, subset(split.secom$test, select = -LABEL))

###########################################################################
# # Meesure of performace.

# table(Actual=testLabel, Fitted=pID3)
# table(Actual=testLabel, Fitted=pRPART)
# table(Actual=testLabel, Fitted=pC50)
# table(Actual=testLabel, Fitted=pSVM)
# table(Actual=testLabel, Fitted=pSVM.allComp)
# table(Actual=y, Fitted=pSVM.original)

measureROC(binning.testLabel, pID3)
measureROC(testLabel, pRPART)
measureROC(testLabel, pC50)
measureROC(testLabel, pSVM)
measureROC(testLabel, pSVM.allComp)
measureROC(split.secom$test$LABEL, pSVM.original)



