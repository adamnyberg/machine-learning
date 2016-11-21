library(kknn)
setwd("~/code/skola/tdde01/adam")

data = read.csv("lab1/spambase.csv")

n=dim(data)[1]
set.seed(12345)
id=sample(1:n, floor(n*0.5))
train=data[id,]
test=data[-id,]

misclass = function(pred, actual) {
  return(mean(pred != actual))
}

classify = function(pred, const=0.5) {
  classified = c()
  for(i in 1:length(pred)) {
    classified[i] = if(pred[i] > const) 1 else 0
  }
  return(classified)
}

distance = function(X,Y) {
  xhat = as.matrix(X)/sqrt(rowSums(X^2))
  yhat = as.matrix(Y)/sqrt(rowSums(Y^2))
  return(1 - xhat%*%t(yhat))
}

knearest = function(data, K, newData, const=0.5) {
  dataSpam = data$Spam
  data$Spam = NULL
  newDataSpam = newData$Spam
  newData$Spam = NULL
  
  D = distance(data, newData)
  
  YpredList = c()
  
  for(i in 1:length(D[1,])) {
    closest = order(D[,i])[1:K]
    Ypred = 0
    
    for(j in closest) {
      if(dataSpam[j] == 1) Ypred = Ypred+1
    }
    
    YpredList[i] = if((Ypred/K) > const) 1 else 0
  }
  return(YpredList)
}



# 1.3-4
pred1 = knearest(train, 1, test)
pred5 = knearest(train, 5, test)

Conf1 = table(pred1, test$Spam)
Mis1 = misclass(pred1, test$Spam)

Conf5 = table(pred5, test$Spam)
Mis5 = misclass(pred5, test$Spam)

# 1.5
kknnPred5 = kknn(Spam~., train, test, 5)
kknnConf5 = table(classify(kknnPred5$fitted.values), test$Spam)
kknnMis5 = misclass(classify(kknnPred5$fitted.values), test$Spam)

# 1.6
tpr=c()
fpr=c()
kktpr=c()
kkfpr=c()

for(bias in seq(0.05, 0.95, 0.05)){
  i = bias*20
  
  nConf = table(knearest(train, 5, test, bias), test$Spam)
  tpr[i] = nConf[2,2]/sum(nConf[,2])
  fpr[i] = nConf[2,1]/sum(nConf[,1])

  kknnConf = table(classify(kknn(Spam~., train, test, 5)$fitted.values, bias), test$Spam)
  kktpr[i] = kknnConf[2,2]/sum(kknnConf[,2])
  kkfpr[i] = kknnConf[2,1]/sum(kknnConf[,1])
}

plot(kkfpr, kktpr, type="l", col="red", xlab = "FPR", ylab = "TPR")
lines(fpr, tpr, col="blue")
legend("topleft", lty=c(1,1), col=c("blue", "red"), legend = c("knearest", "kknn"))



