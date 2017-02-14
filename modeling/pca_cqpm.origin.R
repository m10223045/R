pca.cqpm.origin <- function(secom){
  # 4 variables.
  x1 <- data.frame(cbind(secom$V15, secom$V27, secom$V33, secom$V36))
  
  # 9 variables.
  x2 <- data.frame(cbind(secom$V48, secom$V60, secom$V62, secom$V64, secom$V118, secom$V122, secom$V124, secom$V125, secom$V131))
  
  # 8 variables.
  x3 <- data.frame(cbind(secom$V134, secom$V145, secom$V153, secom$V184, secom$V201, secom$V206, secom$V288, secom$V342))
  
  # 8 variables.
  x4 <- data.frame(cbind(secom$V421, secom$V426, secom$V427, secom$V430, secom$V435, secom$V454, secom$V461, secom$V470))
  
  # 9 variables.
  x5 <- data.frame(cbind(secom$V478, secom$V492, secom$V511, secom$V520, secom$V525, secom$V560, secom$V569, secom$V572, secom$V579))
  
  pca_c1 <- princomp(x1)
  cumulative.PCA.x1 <- summary(pca_c1)
  summary(pca_c1)
  s_c1 <- pca_c1$scores
  c1 <- s_c1[,1:3]
  colnames(c1) <- c("c11","c12","c13")
  
  cx2 <- cbind(c1,x2)
  pca_c2 <- princomp(cx2)
  summary(pca_c2)
  s_c2 <- pca_c2$scores
  c2 <- s_c2[,1:3]
  colnames(c2) <- c("c21","c22","c23")
  # c2 <- s_c2[,1:9]
  # colnames(c2) <- c("c21","c22","c23","c24","c25","c26","c27","c28","c29")
  
  cx3 <- cbind(c2,x3)
  pca_c3 <- princomp(cx3)
  summary(pca_c3)
  s_c3 <- pca_c3$scores
  c3 <- s_c3[,1:5]
  colnames(c3) <- c("c31","c32","c33","c34","c35")
  # c3 <- s_c3[,1:10]
  # colnames(c3) <- c("c31","c32","c33","c34","c35","c36","c37","c38","c39","c310")
  
  cx4 <- cbind(c3,x4)
  pca_c4 <- princomp(cx4)
  summary(pca_c4)
  s_c4 <- pca_c4$scores
  c4 <- s_c4[,1:4]
  colnames(c4) <- c("c41","c42","c43","c44")
  # c4 <- s_c4[,1:17]
  # colnames(c4) <- c("c41","c42","c43","c44","c45","c46","c47","c48","c49","c410","c411","c412","c413","c414","c415","c416","c417")
  
  cx5 <- cbind(c4,x5)
  pca_c5 <- princomp(cx5)
  summary(pca_c5)
  s_c5 <- pca_c5$scores
  c5 <- s_c5[,1:13]
  # c5 <- s_c5[,1:22]
  
  list(c5=c5, pca_c1=pca_c1, pca_c2=pca_c2, pca_c3=pca_c3, pca_c4=pca_c4, pca_c5=pca_c5)
}