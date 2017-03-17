# #  START  # #
setwd("/Volumes/SANDISK_III/project/R/CQPMpca_id3/qrm")

# # Loading function and library
source("resource.R")
loadSources()

# # Subset the secom dataset to only 2 labels
# # Where –1 corresponds to a pass and 1 corresponds to a fail.d
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

cqpmData <- pca.cqpm(x)
data_temp <- cqpmData$c5
data_temp <- data.frame(cbind(data_temp,y))

cqpmData.origin <- pca.cqpm.origin(secom)
data_temp.origin <- cqpmData.origin$c5
data_temp.origin <- data.frame(cbind(data_temp.origin,y))

###########################################################################
# # Plot the Factor Loading.

# dotchart(sort(copm5,decreasing=TRUE), main="Loading Plot for PC1", xlab="Variable Loadings", cex=1.5, col="red")
# plot.FactorLoading(pca4.loading[,3],"comp3")

###########################################################################
# # .

source("combination.R")
comb1(data_temp, y)
comb1(data_temp.origin, y)




