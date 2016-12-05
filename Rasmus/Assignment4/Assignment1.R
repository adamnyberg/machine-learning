setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment4/")
library(readxl)
library(boot)
library(tree)

data=read_excel("xls_state.xls")
n=nrow(data)
set.seed(12345) 

## Task 1 ##
sorted_data = data[with(data, order(data$MET)), ]
plot(sorted_data[,3], sorted_data[,1], xlab="MET", ylab="EX")
data = sorted_data

## Task 2 ##
tree_model = t = tree(data$EX~data$MET, data=data, control=tree.control(minsize=8, nobs=n))
cv_tree_model = cv.tree(tree_model)

# Plotting the cross-validated model, looking for lowest deviance
plot(cv_tree_model)
# Lowest deviance = 3
tree_model = prune.tree(tree_model,best=3)

pred = predict(tree_model, data)
plot(data$MET, data$EX, col="black", xlab="MET", ylab="EX", main="Regression tree predictions vs actual data")
points(data$MET, pred, col="blue")
legend("top", lty=c(1,1), col=c("blue", "black"), legend = c("Predictions", "Data"))

residuals = data$EX - pred
hist(residuals)

## Task 3 ##

# computing bootstrap samples
f = function(data, ind){
  boot_sample = data[ind,]# extract bootstrap sample
  res = tree(EX ~ MET, data=boot_sample, control=tree.control(minsize=8, nobs=n))
  res = prune.tree(tree = res, best = 3)
  #predict values for all Area values from the original
  #data
  pred = predict(res,newdata=sorted_data)
  return(pred)
}
res=boot(data, f, R=1000) #make bootstrap
envelope_non_para = envelope(res, level=0.95) #compute confidence bands

plot(data$MET, data$EX, pch=21, bg="orange", xlab="MET", ylab="EX")

#plot fitted line
points(sorted_data$MET,pred,type="l")
points(sorted_data$MET, envelope_non_para$point[2,], type="l", col="blue")
points(sorted_data$MET, envelope_non_para$point[1,], type="l", col="blue")
title("Non-Parametric Bootstrap")

## Task 4 ##
rng = function(data, mle) {
  data1=data.frame(EX=data$EX, MET=data$MET)
  n=length(data$EX)
  pred = predict(mle, newdata=data1)
  residuals = data1$EX - pred
  data1$EX = rnorm(n, pred, sd(residuals))
  return(data1)
}
f1 = function(data1){
  res = tree(EX ~ MET, data=data1, control=tree.control(minsize=8, nobs=n))
  res = prune.tree(res, best = 3)
  #predict values for all MET values from the original data
  pred=predict(res,newdata=sorted_data)
  return(pred)
}
res=boot(sorted_data, statistic=f1, R=1000,
         mle=tree_model,ran.gen=rng, sim="parametric")

envelope_para = envelope(res, level=0.95) #compute confidence bands

plot(data$MET, data$EX, pch=21, bg="orange", xlab="MET",ylab="EX", ylim=range(100,550))

#plot fitted line
points(sorted_data$MET,pred,type="l")

#plot cofidence bands
points(sorted_data$MET, envelope_para$point[2,], type="l", col="red")
points(sorted_data$MET, envelope_para$point[1,], type="l", col="red")
title("Final result")
points(sorted_data$MET, envelope_non_para$point[2,], type="l", col="blue")
points(sorted_data$MET, envelope_non_para$point[1,], type="l", col="blue")
legend("top", lty=c(1,1), col=c("blue", "red", "purple", "orange", "black"), legend = c("Non-parametric confidence band", "Parametric confidence band", "Prediction bands", "Actual data", "Predictions"))

#Prediction boundary

f2 <- function(data1) {
  tree_model = tree(EX ~ MET, data=data1, control = tree.control(nobs = nrow(data1), minsize = 8))
  tree_model =  prune.tree(tree_model,best=3)
  pred =  predict(tree_model,data1)
  n = length(data$EX)
  ndata = rnorm(n , pred, sd(resid(tree_model)))
  return(ndata)    
}

res = boot(sorted_data, f2,R=1500, mle=tree_model, ran.gen=rng, sim="parametric")
envelope_conf = envelope(res,level=0.95)

points(sorted_data$MET, envelope_conf$point[2,], type="l", col="purple")
points(sorted_data$MET, envelope_conf$point[1,], type="l", col="purple")
