#
preProcessing <- function(x){
  # Processing missing value
  l <- ncol(x)-1
  for(col in 1:l){
    x[is.na(x[, col]), col] <- mean(x[,col], na.rm = T)
  }
  
  # Remove colum that have same value
  for(col in names(x)){
    temp <- x[col]
    if(length(unique(data.matrix(temp)))==1){
      x[col] <- NULL
    }
  }
  
  return(x)
}

#########################################################################################
#
dataBinning <- function(x, class = TRUE){
  if(class)
    ln <- length(x) -1
  else
    ln <- length(x)
  
  ll <- c("very-low", "low", "normal-low", "normal-high", "high", "very-high")
  for(i in 1:ln){
    dataMean <- mean(x[,i])
    dataSD <- sd(x[,i])
    breaks <- c( (-(2*dataSD)+dataMean), (-dataSD+dataMean), dataMean, (dataSD+dataMean), ((2*dataSD)+dataMean))
    f <- cut(x[,i], c(-Inf, breaks, Inf), include.lowest = TRUE, right = FALSE, labels = ll)
    x[,i] <- f 
    # print(summary(f))
  }
  # print(x)
  
  return(x)
}

#########################################################################################
# The dataset must include class attribute.
dataSplit <- function(x, labels, seed = 101, rate = .75, class = TRUE){
  if(!class){
    y <- labels
    x <- cbind(x, y)
  }
    
  set.seed(seed) 
  sample = sample.split(labels, SplitRatio = rate)
  train = subset(x, sample == TRUE)
  test = subset(x, sample == FALSE)
  # print(train)
  # print(test)
  
  list(train = train, test = test)
}

#########################################################################################
# # TEST SPACE
# cuts <- apply(dat, 2, cut, c(-Inf,seq(0.5, 1, 0.1), Inf), labels=0:6)
# tempData <- train
# ll <- c("very-low", "low", "normal-low", "normal-high", "high", "very-high")
# tempData[,1] <- apply(tempData[,1], 2, cut, c(-Inf, breaks, Inf), labels = ll)

# set.seed(101) 
# sample = sample.split(y, SplitRatio = .75)
# train = subset(allComp, sample == TRUE)
# test = subset(allComp, sample == FALSE)
# print(train)
# print(test)


