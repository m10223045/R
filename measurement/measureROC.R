setClass("Measure.ROC",representation(
  pretictTure = 0,
  pretictFalse = 0,
  actualTrue = 0,
  actualFalse = 0
))

setGeneric("measure","Measure.ROC",
            function(p, y){
              for(i in 1:length(p)){
                if( p[i] == y[i] ){
                  
                }else{
                  
                }
              }
            }
           )