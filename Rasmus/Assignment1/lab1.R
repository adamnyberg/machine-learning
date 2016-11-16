library(kknn)

c_func=function(X,Y){
  #X=train, Y=test
  x_part = as.matrix(X)/sqrt(rowSums(X^2))
  y_part = as.matrix(Y)/sqrt(rowSums(Y^2))
  return(x_part %*% t(y_part))
}

d_func=function(X,Y){
  return(1-c_func(X,Y))
}

knearest=function(data,K,newdata){
  #i=test, j=train
  #i test hitta K närmasta train
  #K min i varje kolumn
  #return antal spam / K
  #order
  
  
  
  dmat = d_func(data,newdata)
  result = c()
  
  for (i in 1:1370){
    nearest_neighbor = order(dmat[,i])[1:K]
    nr_of_spam = 0;
    for (index in nearest_neighbor){
      if(spam_train[index] == 1){
        nr_of_spam = nr_of_spam+1
      }
    }
    result[i] = nr_of_spam/K
    
  }
  return(result)
}

confusion_matrix=function(pred_prob) {
  pred_prob = replace(pred_prob, pred_prob < 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob >= 0.5, 1)
  
  conf_mat = table(pred_prob, spam_test)
  
  return(conf_mat)
}

misclass_rate=function(pred_prob) {
  pred_prob = replace(pred_prob, pred_prob < 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob >= 0.5, 1)
  
  n = length(pred_prob)
  mis_class_count = 0
  for (i in 1:n) {
    mis_class_count = mis_class_count + mean(pred_prob[i] != spam_test[i])
  }
  
  return(mis_class_count/n)
}

plot_ROC=function(pred_prob) {
  
  TPR = c()
  FPR = c()
  
  for (i in seq(0.05, 0.95, 0.05)) {
    temp_prob = pred_prob
    temp_prob = replace(temp_prob, temp_prob < i, 0)
    temp_prob = replace(temp_prob, temp_prob >= i, 1)
    
    conf_mat = table(temp_prob, spam_test)
    index = i*20
    TPR[index] = conf_mat[1,1]/sum(conf_mat[1,])
    FPR[index] = conf_mat[2,1]/sum(conf_mat[2,])
  }
  plot(FPR, TPR)
}

setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment1/")

data <- read.csv("spambase.csv")
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

model = train.kknn( Spam~. , data = train, kmax = 5)

spam_train = train$Spam
spam_test = test$Spam
train$Spam = NULL;
test$Spam = NULL;

model_preds = predict(model, test)
confusion_matrix(model_preds)
misclass_rate(model_preds)

