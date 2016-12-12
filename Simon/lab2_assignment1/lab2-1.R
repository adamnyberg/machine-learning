setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab2_assignment1")
set.seed(12345) 
data = swiss
nFolds = 5

linear_regression = function(X,Y){
  X = cbind(1,X)
  Beta = solve((t(X) %*% as.matrix(X))) %*% (t(X)%*%Y)
  print(Beta)
  return(Beta)
}

predict_cv = function(model, testData){
  lin_fnc=model[,1]
  to_be_removed = colnames(testData[,-1])
  to_be_removed = to_be_removed[! to_be_removed %in% names(lin_fnc[-1]), drop = F]
  updated_data = testData[, ! names(testData) %in% to_be_removed, drop = F]
  cv = 0;
  for(i in 1:nrow(testData)){
    predict=sum((lin_fnc[-1])*(updated_data[i,-1]))+lin_fnc[1]
    real = updated_data[i,1]

    cv = cv + (predict-real)^2
  }

  return(cv)
  
}

#Calculate 
cv = function(){
  #Shuffle data
  rdata=data[sample(nrow(data)),]
  #Create 5 folds
  folds <- cut(seq(1,nrow(rdata)),breaks=5,labels=FALSE)
  print(folds)
  best_cv = Inf
  best_subset = "NULL"
  cv_list=c()
  feature_list=c()
  for(j in 1:5){
    subset = combn(names(swiss[,-1]),j, simplify = TRUE)
    
    for (col in 1:ncol(subset)){
      total_cv = 0
      for(i in 1:5){
        testIndexes <- which(folds==i,arr.ind=TRUE)
        testData <- rdata[testIndexes, ]
        trainData <- rdata[-testIndexes, ]
        to_be_removed = colnames(trainData)
        to_be_removed = to_be_removed[! to_be_removed %in% subset[,col], drop = F] 
        subset_data = trainData[, ! names(trainData) %in% to_be_removed, drop = F]
        model = linear_regression(subset_data,trainData[1][,1])
        cv = predict_cv(model,testData)
        total_cv = total_cv+cv
        
      }
      cv_list[length(cv_list)+1]=total_cv
      feature_list[length(feature_list)+1]=length(names(subset_data))
      if(total_cv < best_cv){
        best_cv = total_cv
        #print(subset_data)
        best_subset = names(subset_data)
      }
      #print(total_cv)

    }
  }
  #Display the best CV and subset
  #print(best_cv)
  #print(best_subset)
  
  #Plot all CV scores against # of features
  plot(feature_list,cv_list, xlab = "# of features", ylab="CV")

}
cv()
