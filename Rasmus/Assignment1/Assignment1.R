setwd("C:/Users/rasmu/Documents/Code/tdde01/lab1")

read_data <- read.csv("spambase.csv")
n=dim(read_data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=read_data[id,] 
test=read_data[-id,] 


c_func=function(X,Y){
  x_part = sqrt(t(apply(X, 1, function(x) x / (sum(x^2)) )))
  y_part = sqrt(t(apply(Y, 1, function(y) y / (sum(y^2)) )))
  return(x_part * t(y_part))
}

d_func=function(X,Y){
  return(1 - c_func(X,Y))
}

knearest=function(data,K,newdata){
  return(0)
}