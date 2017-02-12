setwd("/Volumes/SANDISK_III/project/R/CQPMpca_id3/qrm")

# Loading function and library
source("resource.R")

loadSources()
# Subset the secom dataset to only 2 labels
secom <- read.csv("dataset/secom/secom.csv")
secom <- preProcessing(secom)

x <- subset(secom, select = -LABEL)
y <- secom$LABEL

# Feature selection.
x <- fs.ridge(x,y,40)
# fs.svm.x <- fs.svm(secom, y)

# Modeling.
cqpmData <- pca.cqpm(x)
data_temp <- cqpmData$c5
data_temp <- round(data_temp, digits = 6) * 1000000
data_temp <- data.frame(cbind(data_temp,y))

# Split data to train and test (uesing caTools lib).
set.seed(101) 
sample = sample.split(data_temp$y, SplitRatio = .75)
train = subset(data_temp, sample == TRUE)
test = subset(data_temp, sample == FALSE)
testLabel <- test$y
test <- test[,1:22]


# # ChiMerge:Discretization of numeric attributs. (out=c("symb","num"))
library(dprep)
train.disc <- chiMerge(train, 1:22, alpha = 0.05, out = "num")


# # Recursive setting. 
# getOption('expressions')
# options(expressions = 50000)

#################################################################
# # Rule extruction.
# # ID3
modelID3 <- ID3(train,'y')
pID3 <- predict_ID3(train, modelID3)
pID3 <- predict_ID3(test[1,1:22], modelID3)

# # rpart
modelRPART <- rpart(y~., data=train, method = "class")
pRPART <- predict(modelRPART, test, type = "class")
modelRPART.tree <- rpart(y~., data=train)
plot(modelRPART.tree)
text(modelRPART.tree)

# # C5.0
train.part <- train[,1:22]
modelC50 <- C5.0(train.part,as.factor(train$y))
pC50 <- predict(modelC50, test)
plot(modelC50)

# # SVM
modelSVM <- svm(y ~ ., data=train, type='C-classification', kernel='linear')
pSVM <- predict(modelSVM, test)

# # SVM use all compenet from PAC s_c5.
train.all <- data.frame(cbind(cqpmData$pca_c5$scores,y))
modelSVM.all <- svm(y ~ ., data=train.all, type='C-classification', kernel='linear')
pSVM.all <- predict(modelSVM.all, train.all)

# SVM use original secom dataset.
modelSVM.original <- svm(LABEL ~ ., data=secom, type='C-classification', kernel='linear')
pSVM.original <- predict(modelSVM.original, secom)


# table(Actual=testLabel, Fitted=pID3)
table(Actual=testLabel, Fitted=pRPART)
table(Actual=testLabel, Fitted=pC50)
table(Actual=testLabel, Fitted=pSVM)
table(Actual=testLabel, Fitted=pSVM.all)
table(Actual=y, Fitted=pSVM.original)


