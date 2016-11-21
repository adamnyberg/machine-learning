setwd("~/code/skola/tdde01/adam")

data = read.csv("lab1/machines.csv")
set.seed(12345)

# 2.2
# the distrubation type is exponatial
L = function(theta, X) {
  return(apply(theta*exp(-theta%*%t(X)), 1, prod))
}

thetas = seq(0,3, length.out=length(data$Length))

loglike = log(L(thetas, data$Length))
plot(thetas, loglike, col='blue', main="Log-likelihood")
thetasMax = thetas[which(loglike==max(loglike))]

# 2.3
loglike6 = log(L(thetas, data$Length[1:6]))
plot(thetas, loglike, col='blue', main="Log-likelihood")
par(new = TRUE)
plot(thetas, loglike6, col='red', ylab = "", axes = FALSE)
thetasMax6 = thetas[which(loglike6==max(loglike6))]

# 2.4
bl = function(theta, X) {
  return(L(theta, X)*as.vector(L(10, X)))
}

bayesian = log(bl(thetas, data$Length))
plot(thetas, bayesian, col='green')
thetaMaxB = thetas[which(bayesian==max(bayesian))]

# 2.5

expDistr = rexp(50, thetasMax)
hist(expDistr, main="Histogram of exponential distribution")
hist(data$Length, main="Histogram of the original data")







