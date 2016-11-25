setwd("~/code/skola/tdde01/adam/lab2")

data = read.csv("tecator.csv")

set.seed(12345)


n=dim(data)[1]
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

plot(data$Moisture, data$Protein, ylab = "Protein", xlab = "Moisture")
# Yes, the data could be described by a linear model

mse_train_c = c()
mse_test_c = c()

for(i in 1:6) {
  M = lm(Moisture ~ poly(Protein, i), data=train)
  
  mse_train = mean(M$residuals^2)
  mse_train_c[i] = mse_train
  
  pred_test = predict(M, test)
  
  diff = pred_test-test[, "Moisture"]
  
  mse_test = mean(diff^2)
  mse_test_c[i] = mse_test
  
}

plot(seq(1,6), mse_train_c, col = "red", ylim=range(min(mse_train_c, mse_test_c), max(mse_train_c, mse_test_c)), type="o")
points(seq(1,6), mse_test_c, col="blue", type="o")

#2.4 63 variables # Residual standard error: 1.107
library(MASS)
fat_m = lm(Fat ~., data=data[, 2:102])
step = stepAIC(fat_m)



#2.5 coefficients decrease with bigger lamnda, panalty effect
library(glmnet)
covariates=scale(data[,2:101])
response=scale(data[, 102])
r_m=glmnet(as.matrix(covariates), response, alpha=0,family="gaussian")
plot(r_m, xvar="lambda", label=TRUE)



#2.6 optimizes better by removing channels
l_m=glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
plot(l_m, xvar="lambda", label=TRUE)



#2.7 Choose 13 variables
l_cv=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
l_cv$lambda.min
plot(l_cv)
coef(l_cv, s="lambda.min")



#2.8 
#2.4 MSE = 32, 2.7 MSE < 1 Much better 



