setwd("~/code/skola/tdde01/adam")

data = read.csv("lab3/australian-crabs.csv")

#1.1 Scatter plot of (CL) vs. (RW)
data$Color = "red"
data$Color[data$sex == "Male"] = "blue"
plot(data$CL,data$RW,main="CL vs. RW",xlab = "Carapace length",ylab="Rear width",col=data$Color)
#Yes, a line could easily be drawn between them dividing them into different classes.

#ASSIGNMENT 1.2

disc_fun=function(label, S){
X1=X[Y==label,]
#MISSING: compute LDA parameters w1 (vector with 2 values) and w0 (denoted here as b1)

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


# classification
d=res[1]*X[,1]+res[2]*X[,2]+res[3]
Yfit=(d>0)
plot(X[,1], X[,2], col=Yfit+1, xlab="CL", ylab="RW")

#MISSING: use 'res' to plot decision boundary. 

