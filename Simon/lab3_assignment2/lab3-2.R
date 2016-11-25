setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab3_assignment2")
library(e1071)
library(tree)
library(MASS)

set.seed(12345) 
data = read.csv("creditscoring.csv")
n=dim(data)[1]
id=sample(1:n, floor(n*0.5)) 
train_data=data[id,] 
rest_data=data[-id,]
n=dim(rest_data)[1]
id=sample(1:n, floor(n*0.5))
validation_data = rest_data[id,]
test_data = rest_data[-id,]


fit = tree(good_bad~.,data=train_data, split = c("deviance"))
plot(fit)
text(fit,pretty=0)
print(summary(fit))
#misclassification rate for training: 0.2105

test_fit=predict(fit, newdata=test_data, type="class") 
t = table(test_data$good_bad,test_fit)
misclass_rate = 1 - sum(diag(t))/sum(t)

cv.res=cv.tree(fit)
#plot(cv.res)
#plot(cv.res$size, cv.res$dev, type="b",col="red")

trainScore=rep(0,15)
testScore=rep(0,15)
misclass_rates = rep(0,15)
tables = rep(0,15)
for(i in 2:15) {
  prunedTree=prune.tree(fit,best=i)
  pred=predict(prunedTree, newdata=validation_data,
               type="tree")
  trainScore[i]=deviance(prunedTree)
  testScore[i]=deviance(pred)
  Yfit=predict(prunedTree, newdata=validation_data,type="class")
  table = table(validation_data$good_bad,Yfit)
  misclass_rates[i] = 1 - sum(diag(table))/sum(table)
}

best_tree=prune.tree(fit,best=4)
pred=predict(best_tree, newdata=test_data,
             type="tree")
Yfit=predict(best_tree, newdata=validation_data,type="class")
table = table(validation_data$good_bad,Yfit)
best_table = table
misclass_rate = 1 - sum(diag(table))/sum(table)
#print(table)
print(misclass_rate)
plot(2:15, trainScore[2:15], type="b", col="red",ylim=c(250,650), xlab = "# of leaves", ylab = "deviance")
legend("topleft", lty=c(1,1), col=c("red","blue"), legend = c("Train data", "Validation data"))

points(2:15, testScore[2:15], type="b", col="blue")

loss_matrix = matrix(c(0,10,1,0),2)
naive_bayes=naiveBayes(good_bad~., data=train_data, type=c("class"))

#bayes test
Yfit=predict(naive_bayes, newdata=test_data, type="raw")
Yfit[,"bad"] = Yfit[,"bad"]*10
Yfit2 = replace(Yfit, Yfit[,1]>Yfit[,2], "bad")
Yfit2 = replace(Yfit2, Yfit[,1]<=Yfit[,2], "good")
table_test = table(Yfit2[,1],test_data$good_bad)
misclass_test = 1 - sum(diag(table_test))/sum(table_test)

#bayes train
Yfit_train=predict(naive_bayes, newdata=train_data, type="raw")
Yfit_train[,"bad"] = Yfit_train[,"bad"]*10
Yfit2_train = replace(Yfit_train, Yfit_train[,1]>Yfit_train[,2], "bad")
Yfit2_train = replace(Yfit2_train, Yfit_train[,1]<=Yfit_train[,2], "good")
table_train = table(Yfit2_train[,1],train_data$good_bad)
misclass_train = 1 - sum(diag(table_train))/sum(table_train)
