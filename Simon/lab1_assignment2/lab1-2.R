setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab1_assignment2")
data <- read.csv("machines.csv")
set.seed(12345) 



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
  best_theta = lambdas[which(res==max(res))]
  plot(lambdas,res, col = 'blue', ylim=range(res[-1],res_first_six[-1]))
  points(lambdas,res_first_six, col = 'red')
}

bayesian=function(lambdas){
  n = length(data[,1])
  machines_sum = sum(data)
  res=c(1,length(y))
  i = 1
  for(lambda in lambdas){
    res[i] = (lambda^n)*exp(-lambda*machines_sum)
    i = i + 1
  }
  best_theta = lambdas[which(res==max(res))]
  plot(lambdas,res)
}

obs=function(){
  n = 50;
  theta = 1.1
  exp_dist = rexp(n, rate = theta)
  hist(exp_dist)
}



y = seq(0,3,0.1)
#log_likelihood(y)
#bayesian(y)
#hist(data[,1])
obs()
