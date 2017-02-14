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