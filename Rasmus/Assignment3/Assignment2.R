library(tree)
library(MASS) 
library(e1071)

setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment3/")


### Part 1 ###
data <- read.csv("creditscoring.csv")
n=nrow(data)
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
holder=data[-id,]
n=nrow(holder)
id=sample(1:n, floor(n*0.5)) 
validation = holder[id,]
test = holder[-id,]

### Part 2 ###

# Deviance model
  # Misclass on train: 0.2105
  # Misclass on test: 0.284
model = tree(good_bad~., data=train,  split = c("deviance"))

# Gini model
# Misclass on train: 0.2368
# Misclass on test: 0.352
#model = tree(good_bad~., data=train,  split = c("gini") )

print(summary(model))
plot(model)
text(model)
  
test_fit=predict(model, newdata=train, type="class") 
t = table(train$good_bad,test_fit)
misclass_rate = 1 - sum(diag(t))/sum(t)

# Deviance model is the winner.

### Part 3 ###
train_score = rep(0,15) 
validation_score = rep(0,15)
for(i in 2:15) { 
  pruned_tree = prune.tree(model,best=i) 
  pred = predict(pruned_tree, newdata = validation, type="tree") 
  train_score[i] = deviance(pruned_tree) 
  validation_score[i] = deviance(pred) 
} 

plot(2:15, train_score[2:15], type="b", col="red", ylim=c(280,600), xlab="size", ylab="deviance")
points(2:15, validation_score[2:15], type="b", col="blue")
title("Deviance of pruned trees")

final_tree=prune.tree(model, best=4) # seen from graph 
Yfit=predict(final_tree, newdata=test, type="class") 
f_tree_table = table(test$good_bad,Yfit)
f_tree_misclass = 1 - sum(diag(f_tree_table))/sum(f_tree_table)

### Part 4 ###
bayes_model=naiveBayes(good_bad~., data=train, type=c("class"))
Yfit=predict(bayes_model, newdata=train) 
bayes_table = table(Yfit,train$good_bad)
bayes_misclass = 1 - sum(diag(bayes_table))/sum(bayes_table)

  ### Part 5 ###
bayes_model=naiveBayes(good_bad~., data=train, type=c("class"))

# Test prediction
Yfit=predict(bayes_model, newdata=test, type=c("raw"))
Yfit[,1] = Yfit[,1] * 10
Yfit2 = replace(Yfit, Yfit[,1] > Yfit[,2], "bad")
Yfit2 = replace(Yfit2, Yfit[,1] <= Yfit[,2], "good")
bayes_tab = table(Yfit2[,1],test$good_bad)
bayes_misclass = 1 - sum(diag(bayes_tab))/sum(bayes_tab)

# Training prediciton
Yfit=predict(bayes_model, newdata=train, type=c("raw"))
Yfit[,1] = Yfit[,1] * 10
Yfit2 = replace(Yfit, Yfit[,1] > Yfit[,2], "bad")
Yfit2 = replace(Yfit2, Yfit[,1] <= Yfit[,2], "good")
bayes_tab = table(Yfit2[,1],train$good_bad)
bayes_misclass = 1 - sum(diag(bayes_tab))/sum(bayes_tab)
