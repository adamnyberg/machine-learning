y <-seq(from= 0.1, to = 3, by= 0.1)
set.seed(12345) 
#2 och 3

logl<-function(machines,y){
  r = numeric(30)
  r2 = numeric(30)
  machines2 = head(machines,6)
  for(i in 1:30){
    r[i] = dim(machines)[1]*log(y[i]) - y[i]*sum(machines)
  }
  
  for(i in 1:30){
    r2[i] = dim(machines2)[1]*log(y[i]) - y[i]*sum(machines2)
    
  }
  
  r
  plot(y,r, col = 'blue', ylim = range(c(r,r2)))
  points(y,r2, col = 'red')
}

logl(machines,y)

#4
bay<-function(machines,y){
  r3 = numeric(30)
  for(i in 1:30){
    r3[i] = ((y[i]^dim(machines)[1])*exp(-y[i] *sum(machines)))*((10^dim(machines)[1])*exp(-10*sum(machines)))      
  }
  
  plot(y,r3)
}
#bay(machines,y)
#5

newObs<-function(machines){
  newdata<-rexp(n = 50, rate = 1.1)
  hist(newdata)
  hist(machines[,1])
  
}
#newObs(machines)

