setwd("~/code/skola/tdde01/adam")

data = read.csv("lab3/australian-crabs.csv")

#1.1 Scatter plot of (CL) vs. (RW)
data$Color = "black"
data$Color[data$sex == "Male"] = "red"
cols = c("red", "black", "blue")
leg = c("Male", "Female", "Decision boundary")

plot(data$RW, data$CL, ylab = "CL", xlab="RW", col=data$Color, main = "Data, CL vs RW")
legend("topleft", lty=c(1,1), col=c("red", "black"), legend = c("Male", "Female"))

#Yes, a line could easily be drawn between them dividing them into different classes.

#plot(kkfpr, kktpr, type="l", col="red", xlab = "FPR", ylab = "TPR")
#lines(fpr, tpr, col="blue")


#ASSIGNMENT 1.2-3

X = data[5:6]
Y = data$sex

disc_fun=function(label, S){
  X1=X[Y==label,]
  #MISSING: compute LDA parameters w1 (vector with 2 values) and w0 (denoted here as b1)
  Nc = length(X1[,1])
  N = length(X[,1])
  u = colSums(X1)/Nc
  
  b1 = -(1/2)*t(u)%*%solve(S)%*%u + log(Nc/N)
  w1 = solve(S)%*%(u)
  
  return(c(w1[1], w1[2], b1[1,1]))
}

X1=X[Y=="Male",]
X2=X[Y=="Female",]

S=cov(X1)*dim(X1)[1]+cov(X2)*dim(X2)[1]
S=S/dim(X)[1]

#discriminant function coefficients
res1=disc_fun("Male",S)
res2=disc_fun("Female",S)
# MISSING: use these to derive  decision boundary coefficients 'res'

res = res1 - res2

# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
plot(X[,1], X[,2], col=Yfit+1, xlab="RW", ylab="CL", main="Predicted gender")


#MISSING: use 'res' to plot decision boundary.

abline(-res[3]/res[2], -res[1]/res[2], col="blue")
legend("topleft", lty=c(1,1,1), col=cols, legend = leg)

#ASSIGNMENT 1.4

glm_m = glm(sex~RW+CL, family="binomial", data=data)
preds = predict(glm_m)
Yfit2=(preds>0)
plot(X[,1], X[,2], col=Yfit2+1, xlab="RW", ylab="CL", main = "Predicted gender (by glm)")
gcof = glm_m$coefficients
abline(-gcof[1]/gcof[3], -gcof[2]/gcof[3], col="blue")
legend("topleft", lty=c(1,1,1), col=cols, legend=leg)

