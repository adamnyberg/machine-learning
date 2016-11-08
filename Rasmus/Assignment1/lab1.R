library(kknn)

setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment1/")

data <- read.csv("spambase.csv")
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]


## oklartheter##
##model = train.kknn( Spam~. , data = data, train = train, test = test, kmax = 9)
##kknn(formula = formula(train), train, test, k=5)

##model_preds = predict(model, test[, 49])
##confusion_mat = table(test[,49], model_preds)
###

spam_train = train$Spam
spam_test = test$Spam
train$Spam = NULL;
test$Spam = NULL;

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

confusion_matrix=function(data, K, newdata) {
  pred_prob = knearest(data, K, newdata)
  pred_prob = replace(pred_prob, pred_prob < 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob >= 0.5, 1)
  
  conf_mat = table(pred_prob == 1, spam_test == 1)
  
  return(conf_mat)
}

misclass_rate=function(data, K, newdata) {
  pred_prob = knearest(data, K, newdata)
  pred_prob = replace(pred_prob, pred_prob < 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob >= 0.5, 1)
  
  n = length(pred_prob)
  mis_class_count = 0
  for (i in 1:n) {
    mis_class_count = mis_class_count + mean(pred_prob[i] != spam_test[i])
  }
  
  return(mis_class_count/n)
}


