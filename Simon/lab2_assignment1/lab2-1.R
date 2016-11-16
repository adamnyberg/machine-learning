setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab2_assignment1")
set.seed(12345) 
data = swiss
nFolds = 5

#Dependent variable
y = data$Fertility

#Independent variable
x1 = data$Agriculture
x2 = data$Examination
x3 = data$Education
x4 = data$Catholic
x5 = data$Infant.Mortality

linear_regression = function(X,Y){
  X = cbind(1,X)
  Beta = solve((t(X) %*% as.matrix(X))) %*% (t(X)%*%Y)
  return(Beta)
}
l = linear_regression(swiss[-1],y)
predict_cv = function(model, testData){

#  to_be_removed = colnames(trainData)
#  to_be_removed = to_be_removed[! to_be_removed %in% subset[,col], drop = F] 
 # subset_data = trainData[, ! names(trainData) %in% to_be_removed, drop = F]
  
  
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

cv = function(){
  #Randomly shuffle the data
  rdata=data[sample(nrow(data)),]
  #Create 5 equally size folds
  folds <- cut(seq(1,nrow(rdata)),breaks=5,labels=FALSE)
  
  #Perform 5 fold cross validation

  
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
  print(best_cv)
  print(best_subset)
  print(feature_list)
  print(cv_list)
  plot(feature_list,cv_list)
  #Segement your data by fold using the which() function 

  #print(testData)
  
  #return(trainData)
  #print(cv)
  #print(model)
  
}
cv()
