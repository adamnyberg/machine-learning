setwd("~/code/skola/tdde01/adam")

library(readxl)
library(fastICA)
library(pls)

set.seed(12345)

data = read_excel("lab4/NIRSpectra.xls")

# ASSIGNMENT 2.1
data1 = data

data1$Viscosity = c()
res = prcomp(data1)

lambda = res$sdev^2
#eigenvalues lambda
#proportion of variation
sprintf("%2.3f", lambda/sum(lambda)*100)

screeplot(res, main="2.1")
plot(res$x[,1], res$x[,2], ylim=c(-5,15), main="2.1", xlab="PC1", ylab="PC2")

# ASSIGNMENT 2.2
U = res$rotation
plot(U[,1], main="2.2 Traceplot PC1", xlab="index", ylab="", col="blue")
plot(U[,2], main="2.2 Traceplot PC2", xlab="index", ylab="", col="red")

# ASSIGNMENT 2.3
a <- fastICA(data1, 2, alg.typ="parallel", fun="logcosh", alpha=1, method="R", row.norm=FALSE, maxit=200, tol=0.0001, verbose=TRUE)

Wp = a$K%*%a$W
plot(Wp[,1], main="2.3 Traceplot ICA", xlab="index", ylab="PC1", col="blue")
plot(Wp[,2], main="2.3 Traceplot ICA", xlab="index", ylab="PC2", col="red")


layout(matrix(c(1, 2), nrow = 2, ncol = 1))
plot(U[,2], main="2.2 Traceplot PCA PC1 & PC2", xaxt='n', xlab="", ylab="", col="red")
points(U[,1], col="blue")

plot(Wp[,2], main="2.3 Traceplot ICA PC1 & PC2", xlab="index", ylab="", col="red")
points(Wp[,1], col="blue")
legend("bottom", pch=c(1,1), col=c("blue", "red"), legend = c("PC1", "PC2"), xpd = TRUE)
layout(matrix(c(1), 1))

plot(a$S[,1], a$S[,2], main="ICA Latent features", xlab="1", ylab="2")


# ASSIGNMENT 2.4

PCR = pcr(Viscosity~., data=data, validation="CV")
validationplot(PCR, val.type="MSEP", legendpos = "topright", main="2.4 Viscosity CV PCR")




