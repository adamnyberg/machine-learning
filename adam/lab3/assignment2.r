setwd("~/code/skola/tdde01/adam")

library(tree)
library(MASS)
library(e1071)

data = read.csv("lab3/creditscoring.csv")

#ASSIGNMENT 2.1
set.seed(12345) 
n=nrow(data)
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]
id=sample(1:(n/2), floor((n/2)*0.5)) 
validation = test[id,]
test = test[-id,]

#ASSIGNMENT 2.2

mis_rate = function(conf_matrix) {
  return(1 - sum(diag(conf_matrix))/sum(conf_matrix))
}

# DEVIANCE
dev_m = tree(good_bad~., data=train, split = c("deviance"))

dev_fitted_test = predict(dev_m, newdata=test, type="class")
dev_conf_test = table(test$good_bad, dev_fitted_test)
dev_mis_test = mis_rate(dev_conf_test)

dev_fitted_train = predict(dev_m, newdata=train, type="class")
dev_mis_train = mis_rate(table(train$good_bad, dev_fitted_train))


# GINI
gini_m = tree(good_bad~., data=train, split = c("gini"))

gini_fitted_test = predict(gini_m, newdata=test, type="class")
gini_conf_test = table(test$good_bad, gini_fitted_test)
gini_mis_test = mis_rate(gini_conf_test)

gini_fitted_train = predict(gini_m, newdata=train, type="class")
gini_mis_train = mis_rate(table(train$good_bad, gini_fitted_train))

# Choose deviance

#ASSIGNMENT 2.3

tree_m = tree(good_bad~., data=train)
l = 15
trainScore=rep(0,l)
testScore=rep(0,l)
trees = c()
misRate = c()

for(i in 2:l) {
  prunedTree=prune.tree(tree_m,best=i) 
  
  pred=predict(prunedTree, newdata=validation, type="tree")
  trees[i] = pred
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
  
  pred_test=predict(prunedTree, newdata=test, type="class")
  misRate[i]=mis_rate(table(test$good_bad, pred_test))
}

plot(2:l, trainScore[2:l], type="b", col="red", ylim=c(280,600), ylab = "Deviance", xlab = "")
points(2:l, testScore[2:l], type="b", col="blue")
legend("topright", lty=c(1,1,1), col=c("red", "blue"), legend=c("Train", "Test"))

# I'd choose 4 

#ASSIGNMENT 2.4

bayes_m = naiveBayes(good_bad~., data=train, type=c("class"))

bayes_fitted_train = predict(bayes_m, newdata = train) 
bayes_conf_train = table(bayes_fitted_train, train$good_bad)
bayes_mis_train = mis_rate(bayes_conf_train)

bayes_fitted_test = predict(bayes_m, newdata = test) 
bayes_conf_test = table(bayes_fitted_test, test$good_bad)
bayes_mis_test = mis_rate(bayes_conf_test)


#ASSIGNMENT 2.5

classify = function(data) {
  newData = replace(data, data[,1]*10 > data[,2], 'bad')
  newData = replace(newData, data[,1]*10 <= data[,2], 'good')
  return(newData[,1])
}

# Test prediction
b_weight_test = predict(bayes_m, newdata=test, type=c("raw"))
b_weight_test = classify(b_weight_test)
b_weight_conf_test = table(b_weight_test, test$good_bad)
b_weight_mis_test = mis_rate(b_weight_conf_test)

# Training prediciton
b_weight_train = predict(bayes_m, newdata=train, type=c("raw"))
b_weight_train = classify(b_weight_train)
b_weight_conf_train = table(b_weight_train, train$good_bad)
b_weight_mis_train = mis_rate(b_weight_conf_train)
