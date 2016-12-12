setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab3_assignment1")
set.seed(12345) 
data = read.csv("australian-crabs.csv")

#ASSIGNMENT 1.1
female_data = data[!(data$sex=="Male"),]
male_data = data[!(data$sex=="Female"),]
plot(male_data$RW,male_data$CL, col="red", ylab = "CL", xlab="RW", xlim = range(6:20), main = "Assignment 1.1")
points(female_data$RW,female_data$CL, col="black")
legend("topleft", lty=c(1,1), col=c("red","black"), legend = c("Male", "Female"))


#ASSIGNMENT 1.2
X = data[,5:6]
Y = data$sex

disc_fun=function(label, S){
#MISSING: compute LDA parameters w1 (vector with 2 values) and w0 (denoted here as b1)
X1=X[Y==label,]
u = matrix(c(mean(X1$RW),mean(X1$CL)),1)
u = u[1,]
pi = dim(X1)[1]/dim(X)[1]

b1 = -0.5* t(u) %*% solve(S) %*% u+log(pi)
w1 = u%*%solve(S)

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
res = res1-res2
print(res)
# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
plot(X[,1], X[,2], col=Yfit+1, xlab="RW", ylab="CL")
legend("topleft", lty=c(1,1), col=c("red","black"), legend = c("Male", "Female"))
#MISSING: use 'res' to plot decision boundary. 
abline(a=(-res[3])/res[2], b=(-res[1])/res[2])
#Assignment 4
mdl=glm(sex~RW+CL, family="binomial", data=data)
slope = coef(mdl)[2]/(-coef(mdl)[3])
intercept = coef(mdl)[1]/(-coef(mdl)[3]) 
pred = predict(mdl)
Yfit_glm = (pred>0)
plot(X[,1], X[,2], col=Yfit_glm+1, xlab="RW", ylab="CL")
abline(a = intercept, b=slope)
abline(a=(-res[3])/res[2], b=(-res[1])/res[2])
legend("topleft", lty=c(1,1), col=c("red","black"), legend = c("Male", "Female"))

