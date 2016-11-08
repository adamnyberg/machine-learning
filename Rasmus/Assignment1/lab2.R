setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment1/")

machines <- read.csv("machines.csv")

lambdas = seq(0,2,0.1)

exp_log_like = function(data, n, lambdas) {
  answers = c()
  xj_sum = sum(data)
  index = 1
  for(lambda in lambdas) {
    answers[index] = n * log(lambda) - lambda * xj_sum 
    index = index + 1
  }
  return(answers)
}

bayesian_model = function(data, n, lambdas) {
  answers = c()
  xj_sum = sum(data)
  index = 1
  for(lambda in lambdas) {
    answers[index] = (lambda^n) * exp(- lambda * xj_sum )
    index = index + 1
  }
  return(answers)
}

get_best_lambda = function(model, lambdas) {
  lambdas[which(model==max(model))]
}

set.seed(12345)

# Step 2
log_like = exp_log_like(machines, nrow(machines), lambdas)
best_loglike_lambda = get_best_lambda(log_like, lambdas)

# Step 3
log_like_first6 = exp_log_like(machines[,1][1:6], 6, lambdas)

plot(lambdas, log_like, col = "red", ylim=range(exp[-1], exp_first6[-1]))
points(lambdas, log_like_first6, col="blue")

# Step 4
bayes_like_model = bayesian_model(machines, nrow(machines), lambdas)
best_like_lambda = get_best_lambda(bayes_like_model, lambdas)
plot(lambdas, bayes_exp_model)

# Step 5
r = rexp(50, rate=best_loglike_lambda)
hist(r)
