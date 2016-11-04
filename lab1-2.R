y <-seq(from= 0.1, to = 3, by= 0.1)

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
    r3[i] = log((y[i]*exp(-y[i]*sum(machines))* (10*exp(-10*y[i])))) #Osäker på det här specielt på om x ska vara sum(machines) 
  }
  
  
  plot(y,r3)
}

bay(machines,y)