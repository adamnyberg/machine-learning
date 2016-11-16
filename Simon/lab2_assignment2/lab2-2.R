library(MASS)
library(glmnet)
setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab2_assignment2")
set.seed(12345) 
data = read.csv("tecator.csv")
n=dim(data)[1]
id=sample(1:n, floor(n*0.5)) 
train_data=data[id,] 
test_data=data[-id,]

#1
#plot(data[,"Moisture"],data[,"Protein"])

#2
linear_model = function(){
  x_data = c(1:6)
  y_train_MSE = c()
  y_test_MSE = c() 
  validation_diff=c()
  for (i in 1:6){
    lm = lm(Moisture ~ poly(Protein, i), data=train_data)
    MSE = mean(lm$residuals^2)
    predict = predict(lm,test_data)
    diff = predict-test_data[,"Moisture"]
    model_MSE = mean(diff^2)
    print(i)  
    print(MSE)
    print(model_MSE)
    
    y_train_MSE[i] = MSE
    y_test_MSE[i] = model_MSE
  }
  plot(x_data,y_train_MSE, col = "blue", ylim=range(y_train_MSE,y_test_MSE), type = "o", ylab="MSE", xlab="i")
  points(x_data,y_test_MSE, col = 'red', type = "o")
  #overfitting!
}

#4
variable_selection = function(){
  remove = c("Protein", "Moisture", "Sample")
  new_data =  data[, ! names(data) %in% remove, drop = F]
  #lm = lm(Moisture ~ poly(Protein, 1), data=train_data)
  lm = lm(Fat~.,data=data.frame(new_data))
  step = stepAIC(lm, direction="both")
  return(step)
  #63 variables were selected
}
#5
ridge_regression = function(){
  remove = c("Protein", "Moisture", "Sample")
  new_data =  data[, ! names(data) %in% remove, drop = F]
  covariates=scale(new_data[,1:100])
  response=scale(new_data[,101])
  model0=glmnet(as.matrix(covariates),response, alpha=0,family="gaussian")
  plot(model0, xvar="lambda", label=TRUE)

}
#6
remove = c("Protein", "Moisture", "Sample")
new_data =  data[, ! names(data) %in% remove, drop = F]
covariates=scale(new_data[,1:100])
response=scale(new_data[,101])
model0=glmnet(as.matrix(covariates),response, alpha=1,family="gaussian")
#plot(model0, xvar="lambda", label=TRUE)

#7
model=cv.glmnet(as.matrix(covariates),response, alpha=1,family="gaussian")
model$lambda.min
plot(model)
coef(model, s="lambda.min")  

#8
#63  vs 13 variables, Lasso>linear


