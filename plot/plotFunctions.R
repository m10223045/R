# # This part still not complete.
plot.draw <- function(x, y, label){
  library(RColorBrewer)
  label[label==-1] <- 2
  label[label==1] <- 1
  
  cols_t1 <- label
  cols_t1[cols_t1==-1] <- "black"
  cols_t1[cols_t1==1] <- "blue"
  
  #######
  # cols <- brewer.pal(12, "Paired")
  # cols_t1 <- cols[label]
  # cols_t1 <- c("red","blue")
  
  plot( x, y, pch=label, col = cols_t1)
}

plot.FactorLoading <- function(data, name = "Factor", cex=1.5, col="red", DESC=TRUE){
  #dotchart(sort(copm5,decreasing=TRUE), main="Loading Plot for PC1", xlab="Variable Loadings", cex=1.5, col="red")
  main <- paste("Loading Plot for",name)
  dotchart(sort(data,decreasing=DESC), main=main, xlab="Variable Loadings", cex=1.5, col="red")
}