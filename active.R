setwd("/Volumes/SANDISK_III/project/R/CQPMpca_id3/qrm")
source("source.R")

loadSources()
# Subset the secom dataset to only 2 labels
secom <- read.csv("dataset/secom/secom.csv")
secom <- preProcessing(secom)

x <- subset(secom, select = -LABEL)
y <- secom$LABEL

#sort(fs.svm(x,y))
x <- fs.ridge(x,y,40)
fs.svm.x <- fs.svm(secom, y)

# Modeling
# afterPCA <- pca.cqpm(x)
# train <- afterPCA[1]
train <- pca.cqpm(x)

train <- data.frame(cbind(train,y))

# Rule extruction
# modelID3 <- ID3(train,'y')
modelID3 <- ID3(train,'y')
pID3 <- predict_ID3(testing,modelID3)
# pID3 <- predict_ID3(train,modelID3)

modelSVM <- svm(y ~ ., data=train, type='C-classification', kernel='linear')
pSVM <- predict(modelSVM, testing)
# pSVM <- predict(modelSVM, train)

###
train.all <- data.frame(cbind(s_c5,y))
modelSVM.all <- svm(y ~ ., data=train.all, type='C-classification', kernel='linear')
pSVM.all <- predict(modelSVM.all, train.all)
table(Actual=y, Fitted=pSVM.all)
###


table(Actual=y, Fitted=pID3)
table(Actual=y, Fitted=pSVM)


###
modelSVM.orignal <- svm(LABEL ~ ., data=secom, type='C-classification', kernel='linear')
pSVM.orignal <- predict(modelSVM.orignal, secom)
table(Actual=y, Fitted=pSVM.orignal)
###

