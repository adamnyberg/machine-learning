library(neuralnet)

set.seed(1234567890)
Var = runif(50, 0, 10)
trva = data.frame(Var, Sin=sin(Var))

train = trva[1:25,] # Training
validation = trva[26:50,] # Validation


# Random initializaiton of the weights in the interval [-1, 1]
winit = runif(31, -1, 1)# Your code here

MSEs = c()
bestMSE = Inf
bestNN = Inf
bestThreshold = 0

for(i in 1:10) {
  nn = neuralnet(Sin~Var, data=train, hidden = 10, threshold = (i/1000), startweights = winit)
    
  # Your code here
  preds = compute(nn, validation$Var)$net.result
  real = validation$Sin
  MSEs[i] = sum((real - preds)^2)/nrow(validation)
  
  if (MSEs[i] < bestMSE) {
    bestNN = nn
    bestMSE = MSE
    bestThreshold = i/1000
  }
}

plot(bestNN)

plot(1:10, MSEs, ylab = "MSE", xlab="Threshold", main = "Choosing threshold")

# Plot of the predictions (black dots) and the data (red dots)
plot(prediction(bestNN)$rep1, col="blue", ylab="", xlab="")
points(trva, col = "red")
legend("bottomleft", pch=c(1,1), col=c("blue", "red"), legend = c("NN prediction", "sin(x)"), xpd = TRUE)


# Plot predicted over sequence
interval = seq(0, 8*pi, 0.1)
plot(interval, compute(bestNN, interval)$net.result, type="l", col="blue",  ylab="", xlab="", main="NN predriction over a large interval")
points(interval, sin(interval), type="l", col="red")
legend("bottomleft", pch=c(1,1), col=c("blue", "red"), legend = c("NN prediction", "sin(x)"), xpd = TRUE)



