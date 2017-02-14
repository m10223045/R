
pca.cqpm <- function(x){

  x1 <- x[,1:4]
  pca_c1 <- princomp(x1)
  s_c1 <- pca_c1$scores
  c1 <- s_c1[,1:3]
  colnames(c1) <- c("c11","c12","c13")

  x2 <- x[,5:13]
  cx2 <- cbind(c1,x2)
  pca_c2 <- princomp(cx2)
  s_c2 <- pca_c2$scores
  c2 <- s_c2[,1:9]
  colnames(c2) <- c("c21","c22","c23","c24","c25","c26","c27","c28","c29")

  x3 <- x[,14:23]
  cx3 <- cbind(c2,x3)
  pca_c3 <- princomp(cx3)
  s_c3 <- pca_c3$scores
  c3 <- s_c3[,1:10]
  colnames(c3) <- c("c31","c32","c33","c34","c35","c36","c37","c38","c39","c310")

  x4 <- x[,24:31]
  cx4 <- cbind(c3,x4)
  pca_c4 <- princomp(cx4)
  s_c4 <- pca_c4$scores
  c4 <- s_c4[,1:17]
  colnames(c4) <- c("c41","c42","c43","c44","c45","c46","c47","c48","c49","c410","c411","c412","c413","c414","c415","c416","c417")

  x5 <- x[,32:40]
  cx5 <- cbind(c4,x5)
  pca_c5 <- princomp(cx5)
  s_c5 <- pca_c5$scores
  c5 <- s_c5[,1:22]

  list(c5=c5, pca_c1=pca_c1, pca_c2=pca_c2, pca_c3=pca_c3, pca_c4=pca_c4, pca_c5=pca_c5)
}

