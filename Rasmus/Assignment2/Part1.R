set.seed(12345)
setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment2/")


linear_reg = function(X, Y) {
  
  # bind 1s to X to get intercept
  X = cbind(1,X)
  
  Beta = solve((t(X) %*% as.matrix(X))) %*% (t(X)%*%Y)
  return(Beta)
}

calc_CV = function(model, testData, combination) {
  
  CV = 0
  complete_model = c(0,0,0,0,0)
  
  intersect = model[1]
  # Remove model intercect
  model = model[-1]
  
  # Move model data to correct position relative to testData
  for(index in combination) {
    complete_model[index] = model[1]
    model = model[-1]
  }
  
  # Vector with correct true values
  true_answer = testData[1][,1]
  
  testData = testData[-1]
  diff = as.matrix(testData) %*% diag(complete_model)
  for(i in 1:nrow(testData)) {
    CV = CV + (intersect + sum(diff[i,]) - true_answer[i])^2  
  }
  return(CV)
}


#Randomly shuffle the data
yourData<-swiss[sample(nrow(swiss)),]

number_of_folds = 5
folds <- cut(seq(1,nrow(yourData)),breaks=numberOfFolds,labels=FALSE)

best_subset = c()
best_CV = Inf

# Create all feature combinations
feat_set1 = combn(c(1,2,3,4,5),1)
feat_set2 = combn(c(1,2,3,4,5),2)
feat_set3 = combn(c(1,2,3,4,5),3)
feat_set4 = combn(c(1,2,3,4,5),4)
feat_set5 = combn(c(1,2,3,4,5),5)

feature_set = list(feat_set1, feat_set2, feat_set3, feat_set4, feat_set5)

# Final data added to these vectors in order to plot
CV_scores_axis = c()
features_axis = c()

for(amount_features in 1:5) {
  
  # f_set contains all feature-combinations of size amount_features
  f_set = feature_set[[amount_features]]
  
  for(feature_set_index in 1:ncol(f_set)){
    feature_indexes = f_set[,feature_set_index]
    CV = 0
    for(i in 1:number_of_folds) {
      testIndexes <- which(folds==i,arr.ind=TRUE)
      testData <- yourData[testIndexes, ]
      trainData <- yourData[-testIndexes, ]
      model = linear_reg(trainData[-1][feature_indexes], trainData[1][,1])
      CV = CV + calc_CV(model, testData, feature_indexes)
    }
    CV_scores_axis[length(CV_scores_axis)+1] = CV
    features_axis[length(features_axis)+1] = length(feature_indexes)
    if (CV < best_CV) {
      best_subset = feature_indexes
      best_CV = CV
    }
  }
}