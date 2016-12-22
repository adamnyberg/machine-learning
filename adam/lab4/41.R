setwd("~/code/skola/tdde01/adam")

library(readxl)
library(tree)
library(boot)
set.seed(12345)

unsorted_data = read_excel("lab4/State.xls")

# ASSIGNMENT 1.1
data = unsorted_data[with(unsorted_data, order(unsorted_data$MET)), ] # HAXXXOR
n = nrow(data)

plot(data$MET, data$EX, ylab="EX", xlab="MET", main="1.1")#, ylim = range(150,500), xlim = range(0,350))


# ASSIGNMENT 1.2

tree_m = tree(data$EX~data$MET, data=data, control=tree.control(n, minsize=8))

tree_m_cv <- cv.tree(object=tree_m)
k = tree_m_cv$size[which(tree_m_cv$dev==min(tree_m_cv$dev))]
tree_m_pruned <- prune.tree(tree_m, best=k)

plot(tree_m_cv)

plot(tree_m_pruned, main="1.2 Tree model")
text(tree_m_pruned, main="1.2 Tree model")

tree_pred_ex = predict(tree_m_pruned, data)

plot(data$MET, data$EX, ylab="EX", xlab="MET", main="1.2 Fitted data")
points(data$MET, tree_pred_ex, ylab="EX", xlab="MET", col="blue")
legend("top", pch=c(1,1), col=c("black", "blue"), legend = c("data", "tree_fitted"))

tree_m_residuals = data$EX-tree_pred_ex

hist(tree_m_residuals, main="1.2 Histogram of residuals", xlab="Residuals")

# ASSIGNMENT 1.3

# computing bootstrap samples
bootstrap=function(in_data, ind){
  data1=in_data[ind,]# extract bootstrap sample
  model=tree(EX~MET, data=data1, control=tree.control(n, minsize=8))
  model_pruned <- prune.tree(model, best=k)

  priceP=predict(model_pruned,newdata=data)
  return(priceP)
}

boot_res=boot(data, bootstrap, R=1000) #make bootstrap

env=envelope(boot_res, level=0.95) #compute non-parametric confidence bands

plot(data$MET, data$EX, pch=21, main="1.3-4 Bootstrap", xlab="MET", ylab="EX", ylim=range(100,500))
points(data$MET, tree_pred_ex, type="l") #plot fitted line
#plot cofidence bands
points(data$MET, env$point[2,], type="l", col="blue")
points(data$MET, env$point[1,], type="l", col="blue")


# ASSIGNMENT 1.4

# Parametric confidence bands
rng=function(in_data, model) {
  data1=data.frame(EX=in_data$EX, MET=in_data$MET)
  n=length(in_data$EX)
  pred = predict(model, newdata=data1)
  data1$EX=rnorm(n, pred, sd(data$EX-pred))
  return(data1)
}

bootstrap_2=function(data1){
  model=tree(EX~MET, data=data1, control=tree.control(n, minsize=8))
  model_pruned <- prune.tree(model, best=k)
  priceP=predict(model_pruned, newdata=data)
  return(priceP)
}

boot_parametric_res=boot(data, statistic=bootstrap_2, R=1000, mle=tree_m, ran.gen=rng, sim="parametric")

env_parametric=envelope(boot_parametric_res, level=0.95)

points(data$MET, tree_pred_ex, type="l") #plot fitted line
points(data$MET, env_parametric$point[2,], type="l", col="red")
points(data$MET, env_parametric$point[1,], type="l", col="red")


# Prediction boundary
bootstrap_3 = function(data1) {
  model = tree(EX~MET, data=data1, control = tree.control(nobs = nrow(data1), minsize = 8))
  model_pruned = prune.tree(model, best=3)
  pred = predict(model_pruned, data1)
  n = length(data1$EX)
  ndata = rnorm(n, pred, sd(resid(model_pruned)))
  return(ndata)    
}

boot_prediction = boot(data, bootstrap_3, R=1000, mle=tree_m_pruned, ran.gen=rng, sim="parametric")
boot_prediction_env = envelope(boot_prediction, level=0.95)

points(data$MET, boot_prediction_env$point[2,], type="l", col="green")
points(data$MET, boot_prediction_env$point[1,], type="l", col="green")


legend("top", lty=c(1,1), col=c("black", "blue", "red", "green"), legend = c("Fitted line", "Non-parametric band", "Parametric band", "Prediction boundaries"))




