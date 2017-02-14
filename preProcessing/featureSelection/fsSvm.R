# seed<- 123
# 
# train<-sim.data(n = 200, ng = 100, nsg = 10, corr=FALSE, seed=seed )
# print(str(train)) 
# 
# 
# ### Fixed grid ####
# 
# # train SCAD SVM ####################
# # define set values of tuning parameter lambda1 for SCAD 
# lambda1.scad <- c (seq(0.01 ,0.05, .01),  seq(0.1,0.5, 0.2), 1 ) 
# # for presentation don't check  all lambdas : time consuming! 
# lambda1.scad<-lambda1.scad[2:3]
# # 
# # train SCAD SVM
# data.matrix()
# # computation intensive; for demostration reasons only for the first 100 features 
# # and only for 10 Iterations maxIter=10, default maxIter=700
# #fs.method = c("scad", "1norm", "scad+L2", "DrHSVM"),

fs.svm <- function(x,y){
  # require(penalizedSVM)
  
  seed<- 123
  
  lambda1.scad <- c (seq(0.01 ,0.05, .01),  seq(0.1,0.5, 0.2), 1 )
  lambda1.scad <- lambda1.scad[2:3]
  
  # system.time(scad.fix<- svm.fs(t(train$x)[,1:100], y=train$y, fs.method="scad",
  system.time(test.fix <- svm.fs(as.matrix(x), y, fs.method="scad",
                                cross.outer= 0, grid.search = "discrete",  
                                lambda1.set=lambda1.scad,
                                parms.coding = "none", show="none",
                                maxIter = 10, inner.val.method = "gacv", cross.inner= 10,
                                seed=seed, verbose=FALSE) 	)
  
  fs.names <- NULL
  fs.value <- NULL
  for(n in 1:length(test.fix$model$xind)){
    # fs.names <- c(fs.names, names(x)[n])
    fs.names <- c(fs.names, names(x)[n])
    fs.value <- c(fs.value, test.fix$model$w[n])
  }
  
  fs.value <- data.frame(t(abs(fs.value)))
  colnames(fs.value) <- c(fs.names)
  
  return(fs.value)
}






# rprint(scad.fix)
