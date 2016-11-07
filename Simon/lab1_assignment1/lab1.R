data <- read.csv("spambase.csv")
n=dim(data)[1]
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

c_func=function(X,Y){
  #X=train, Y=test
  x_part = as.matrix(X)/sqrt(rowSums(X^2))
  y_part = as.matrix(Y)/sqrt(rowSums(Y^2))
  return(x_part %*% t(y_part))
}

d_func=function(X,Y){
  return(1-c_func(X,Y))
}

knearest=function(data,K,newdata){
  #i=test, j=train
  #i test hitta K närmasta train
  #K min i varje kolumn
  #return antal spam / K
  #order
  spam_data = data$Spam
  print(spam_data)
  spam_newdata = newdata$Spam
  data$Spam = NULL;
  newdata$Spam = NULL;
  print(spam_data)
  
  
  dmat = d_func(data,newdata)
  result = c()
  
  for (i in 1:1370){
    nearest_neighbor = order(dmat[,i])[1:K]
    nr_of_spam = 0;
    for (index in nearest_neighbor){
      print(index)
      if(spam_data[index] == 1){
        nr_of_spam = nr_of_spam+1
      }
    }
    result[i] = nr_of_spam/K
    
  }
  return(result)
  
}



