setwd("~/code/skola/tdde01/adam")

realdata = read.csv("spambase2.csv")
testData <- matrix(c(
  0, 1, 1, 2,
  0, 0, 1, 3,
  0, 0, 1, 3
), nrow = 3, byrow = TRUE)


K=1
data=realdata


n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

x_part = sqrt(t(apply(train, 1, function(x) x / sum(x^2) )))
y_part = sqrt(t(apply(train, 1, function(y) y / sum(y^2) )))

C=x_part * y_part

D=1-C


#classified <- knearest(data, K, newdata){
#  statements
#  return(object)
#}

