library(kknn)
setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab1_assignment1")
data <- read.csv("spambase.csv")

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
  pred_prob = replace(pred_prob, pred_prob <= 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob > 0.5, 1)
  conf_mat = table(pred_prob, spam_test)
  return(conf_mat)
}


misclass_rate=function(pred_prob) {
  pred_prob = replace(pred_prob, pred_prob <= 0.5, 0)
  pred_prob = replace(pred_prob, pred_prob > 0.5, 1)
  
  n = length(pred_prob)
  mis_class_count = 0
  for (i in 1:n) {
    mis_class_count = mis_class_count + mean(pred_prob[i] != spam_test[i])
  }
  
  return(mis_class_count/n)
}

plot_ROC=function(pred_prob, pred2){
  TPR=c()
  FPR=c()
  TPR2=c()
  FPR2=c()
  pi_values = seq(0.05,0.95,0.05)
  for(i in pi_values){
    new_prod = pred_prob
    new_prod = replace(new_prod, new_prod < i, 0)
    new_prod = replace(new_prod, new_prod >= i, 1)
    conf_mat = table(spam_test, new_prod)
    index = i*20
    print(conf_mat)
    FPR[index] = conf_mat[1,2]/sum(conf_mat[1,])
    TPR[index] = conf_mat[2,2]/sum(conf_mat[2,])
    
    new_prod2 = pred2
    new_prod2 = replace(new_prod2, new_prod2 < i, 0)
    new_prod2 = replace(new_prod2, new_prod2 >= i, 1)
    conf_mat2 = table(spam_test, new_prod2)
    FPR2[index] = conf_mat2[1,2]/sum(conf_mat2[1,])
    TPR2[index] = conf_mat2[2,2]/sum(conf_mat2[2,])
  }
  plot(FPR,TPR, type="l",main="ROC curve of our K-NN", col = "red")
  lines(FPR2,TPR2, col="blue")
}


n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

result = kknn(Spam~.,train, test,k=5)
spam_train = train$Spam
spam_test = test$Spam
train$Spam = NULL;
test$Spam = NULL;

prediction = predict(result)
confusion_matrix(prediction)
misclass_rate(prediction)
plot_ROC(prediction)
plot_ROC(knearest(train,5,test), prediction)
legend("topleft", lty=c(1,1), col=c("red","blue"), legend = c("My K-NN", "kknn"))
#misclass_rate(knearest(train,1,test))
#misclass_rate(knearest(train,5,test))
#confusion_matrix(knearest(train,1,test))
#confusion_matrix(knearest(train,5,test))

#misclass_rate(knearest(train,1,train))
#confusion_matrix(knearest(train,1,train))




