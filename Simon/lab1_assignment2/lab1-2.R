setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab1_assignment2")
data <- read.csv("machines.csv")


log_likelihood=function(lambdas){
  n = length(data[,1])
  n_first_six = 6
  machines_sum = sum(data)
  machines_sum_first_six = sum(data[,1][1:6])
  res=c(1,length(y))
  res_first_six=c(1,length(y))
  i = 1
  for(lambda in lambdas){
    res[i] = n*log(lambda)-lambda*machines_sum
    res_first_six[i] = n_first_six*log(lambda)-lambda*machines_sum_first_six
    i = i + 1
  }
  print(res_first_six)
  best_theta = y[which(logl==max(logl))]
  plot(y,res, col = 'blue', ylim=range(res[-1],res_first_six[-1]))
  points(y,res_first_six, col = 'red')
}

bayesian=function(){
  
}



y = seq(0,3,0.1)
log_likelihood(y)
