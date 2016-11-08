setwd("C:/Users/rasmu/Documents/Code/tdde01/tdde01/Rasmus/Assignment1/")

machines <- read.csv("machines.csv")

lambdas = seq(0,2,0.1)

exp_log_like = function(data, data_length, lambdas) {
  n = data_length
  answers = c()
  xj_sum = sum(data)
  index = 1
  for(lambda in lambdas) {
    answers[index] = n * log(lambda) - lambda * xj_sum 
    index = index + 1
  }
  return(answers)
}

#exp_log = exp_log_like(machines,lambdas)
#plot(lambdas, exp_log_like(machines,lambdas))

# Seen from the plot
best_result = 1.1

plot(lambdas, exp_log_like(machines, nrow(machines), lambdas), col = "red")
points(lambdas, exp_log_like(machines[,1][1:6], 6, lambdas), col="blue")

#plot(y,r, col = 'blue', ylim = range(c(r,r2)))
#points(y,r2, col = 'red')