library(fastICA)
library(pls)

data=read_excel("NIRSpectra.xls")
n=nrow(data)
set.seed(12345) 

## Task 1 ##
data1 = data
data1$Viscosity = c()

# PC1, PC2, .. 
res = prcomp(data1)
lambda = res$sdev^2
#eigenvalues lambda
#proportion of variation

# Print with correct numbers of decimals
sprintf("%2.3f",lambda/sum(lambda)*100)

screeplot(res, main="Variation explained by each feature")

# Two components is sufficient to describe 99 %.

plot(res$x[,1], res$x[,2], xlab="PC1", ylab="PC2", ylim=range(-0.3,0.3), main="Data in PC1 and PC2")


c1 = "orange"
c2 = "pink"

## Task 2 ##
U=res$rotation
plot(U[,2],main="Traceplot of PC1 and PC2", pch=21, bg=c2, ylab="")
points(U[,1], main="Traceplot, PC1", pch=21, bg=c1)
legend("topleft", pch=c(16,16), col=c(c1, c2), legend = c("PC1", "PC2"))

## Task 3 ##
# Task A #
a <- fastICA(data1, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) #ICA
W1 = a$K %*% a$W
plot(W1[,2],main="Traceplot of IC1 and IC2",  pch=21, bg=c2, ylab="")
points(W1[,1], main="Traceplot, IC1", pch=21, bg=c1)
legend("bottomleft", pch=c(16,16), col=c(c1, c2), legend = c("PC1", "PC2"))

# Task B #
plot(a$S[,1], a$S[,2], xlab="Featuer 1", ylab = "Feature 2")
title("Latent features")
## Task 4 ##
pcr.fit=pcr(Viscosity~., data=data, validation="CV")
validationplot(pcr.fit,val.type="MSEP")

# 20 components seems reasonable.