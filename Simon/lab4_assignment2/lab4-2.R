setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab4_assignment2")
set.seed(12345) 
library(readxl)
library(fastICA)
library(pls)
data=read_excel("NIRSpectra.xls")

#2.1
data1 = data
data1$Viscosity=c()
res=prcomp(data1)
lambda=res$sdev^2
#eigenvalues
lambda
#proportion of variation
sprintf("%2.3f",lambda/sum(lambda)*100)
PC = res
screeplot(PC)
U=res$rotation
head(U)
plot(res$x[,1], res$x[,2], ylim=c(-1,1), xlab = "PC1", ylab = "PC2")

#2.2
plot(U[,2],main="2.2 Traceplot",xlab = "index",ylab="", bg ="blue", pch=21)
points(U[,1], bg ="orange", pch=21)
legend("topleft", pch=c(16,16), col=c("orange","blue"), legend = c("PC1", "PC2"))


#2.3
a <- fastICA(data1, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200, tol = 0.0001, verbose = TRUE) #ICA

Wp = a$K %*% a$W
plot(Wp[,2],main="2.3 fastICA traceplot",xlab = "index",ylab="", bg ="blue", pch=21)
legend("bottomleft", pch=c(16,16), col=c("orange","blue"), legend = c("IC1", "IC2"))
points(Wp[,1], bg="orange", pch=21)

plot(a$S[,1], a$S[,2], ylim = c(-5,5), xlab = "1", ylab = "2", main = "ICA latent features")

#2.4

pcr.fit=pcr(Viscosity~., data=data, validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP")

#select 20 components, good MSEP and low complexity

