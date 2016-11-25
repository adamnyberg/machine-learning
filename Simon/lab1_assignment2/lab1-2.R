setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab1_assignment2")
data <- read.csv("machines.csv")
set.seed(12345) 

log_likelihood=function(thetas){
  n = length(data[,1])
  n_first_six = 6
  machines_sum = sum(data)
  machines_sum_first_six = sum(data[,1][1:6])
  
  res=c(1,length(y))
  res_first_six=c(1,length(y))
  i = 1
  for(theta in thetas){
    res[i] = n*log(theta)-theta*machines_sum
    res_first_six[i] = n_first_six*log(theta)-theta*machines_sum_first_six
    i = i + 1
  }
  #print(res)
  
  best_theta = thetas[which(res==max(res))]
  theta = thetas #make output look better
  log_like = res #make output look better
  plot(theta,log_like, col = 'blue', ylim=range(res[-1],res_first_six[-1]), main="Log-likelihood")
  points(theta,res_first_six, col = 'red')
}

bayesian=function(thetas){
  n = length(data[,1])
  machines_sum = sum(data)
  res=c()
  new_res=c()
  #res=c(1,length(y))
  i = 1
  for(theta in thetas){
    res[i] = (theta^n)*exp(-theta*machines_sum)*(10^n)*exp(-10*machines_sum)
    new_res[i] = (n*log(theta)-theta*machines_sum)+log(10*exp(-10*theta))
    i = i + 1
  }
  #print(res)
  #print(new_res)
  theta = thetas[-1] #make output look better
  likelihood = new_res[-1] #make output look better
  best_theta = thetas[which(likelihood==max(likelihood))]
  plot(theta,likelihood, main = "Bayesian Likelihood", type="l", ylab="Marginal Likelihood")
}

obs=function(){
  n = 50;
  theta = 1.1
  exp_dist = rexp(n, rate = theta)
  hist(exp_dist, main="Histogram of exponential distribution")
}

y = seq(0,3,0.0001)
#log_likelihood(y)
bayesian(y)
#lifespan = data[,1]
#hist(lifespan, main="Histogram of the original data")
#obs()
