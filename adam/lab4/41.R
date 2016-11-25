setwd("~/code/skola/tdde01/adam")

data = read.csv("lab4/State.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]
