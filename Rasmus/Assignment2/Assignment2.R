setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment2/")
library(MASS)
library
set.seed(12345)
data = read.csv("tecator.csv")
n=dim(data)[1]
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

# # # Assignment 1 # # #
plot(data$Moisture,data$Protein)

# Looks linear!

# # # Assignment 2 and 3 # # #
MSE_train_scores = c()
MSE_test_scores = c()
for(i in 1:6){
  Mi = lm(Moisture ~ poly(Protein, i), data=train)
  
  MSE_train = mean(Mi$residuals^2)
  
  moisture_prediction = predict(Mi,test)
  MSE_test = mean((moisture_prediction - test$Moisture)^2)
  
  MSE_train_scores[i] = MSE_train
  MSE_test_scores[i] = MSE_test
}

plot(seq(1,6), MSE_train_scores, col = "red", ylim=range(min(MSE_train_scores, MSE_test_scores), max(MSE_train_scores, MSE_test_scores)), type="o")
points(seq(1,6), MSE_test_scores, col="blue", type="o")

# # # Assignment 4 # # #
model = lm(Fat~., data=data[2:102])
steps = stepAIC(model)
#length(steps$coefficients) # = 64 (one is intercept, so 64 vars)

# # # Assignment 5 # # #
covariates=scale(data[2:101]) 
response=scale(data[102])
ridge_model = glmnet(as.matrix(covariates), response, alpha=0,family="gaussian") 
plot(ridge_model, xvar="lambda", label=TRUE)

# # # Assignment 6 # # #
# Alpha set to 1 in order to obtain Lasso

lasso_model = glmnet(as.matrix(covariates), response, alpha=1,family="gaussian") 
plot(lasso_model, xvar="lambda", label=TRUE)

cv_lasso_model=cv.glmnet(as.matrix(covariates), response, alpha=1,family="gaussian")
cv_lasso_model$lambda.min 
plot(cv_lasso_model) 
coef(cv_lasso_model, s="lambda.min")

