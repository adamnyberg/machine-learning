disc_fun=function(label, S){
  X1=X[Y==label,]
  X_sum = colSums(X1)
  
  # According to formulas from slides
  Nc = nrow(X1)
  N = nrow(X)
  pi = Nc/N
  u = X_sum / Nc
  w1 = solve(S) %*% u
  b1 = -(0.5) * t(u) %*% solve(S) %*% u + log(pi)

  return(c(w1[1], w1[2], b1[1,1]))
}

data <- read.csv("australian-crabs.csv")
n=nrow(data)
set.seed(12345) 
id=sample(1:n, floor(n*0.5)) 
train=data[id,] 
test=data[-id,]

X = data[5:6]
Y = data$sex

X1=X[Y=="Male",]
X2=X[Y=="Female",]

plot(X1$RW, X1$CL, xlab="RW", ylab="CL", main="Difference between males and females", col="red", ylim = range(min(X1$CL, X2$CL), max(X1$CL, X2$CL)))
points(X2$RW, X2$CL, col="black")
legend("topleft", lty=c(1,1), col=c("red", "black"), legend = c("Male", "Female"))

S = cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
S = S/dim(X)[1]

#discriminant function coefficients
res1=disc_fun("Male",S)
res2=disc_fun("Female",S)

res = res1 - res2

# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
plot(X[,1], X[,2], col=Yfit+1, xlab="RW", ylab="CL", main = "Predicted gender")
legend("topleft", lty=c(1,1), col=c("red", "black"), legend = c("Male", "Female"))


# res[1] * RW + res[2] * CL + res[3] = 0
# CL = - (res[1] * CL + res[3]) / res[2]
abline(a=-res[3]/res[2], b=-res[1]/res[2], col="purple")

glm_model = glm(sex~RW+CL, family="binomial", data=data)
preds = predict(glm_model)
Yfit2=(preds>0)
plot(X[,1], X[,2], col=Yfit2+1, xlab="RW", ylab="CL", main = "Predicted gender (by glm)")
legend("topleft", lty=c(1,1), col=c("red", "black"), legend = c("Male", "Female"))
gcof = glm_model$coefficients
abline(-gcof[1]/gcof[3], -gcof[2]/gcof[3], col="blue")
