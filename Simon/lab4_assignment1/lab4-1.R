setwd("/Users/Simon/Documents/TDDE01/tdde01/Simon/lab4_assignment1")
library(readxl)
library(tree)
library(boot)
set.seed(12345) 
data=read_excel("xls_state.xls")

#1.1
reorder_data = data[with(data, order(data$MET)), ]
plot(reorder_data$MET,reorder_data$EX, ylab = "EX", xlab = "MET", main="1.1 EX vs MET")
#---->regression tree model

#1.2
MET = reorder_data$MET # make output look better
model = tree(reorder_data$EX~MET, data=reorder_data, 
             control = tree.control(minsize = 8, nobs = nrow(reorder_data)))
model.cv <- cv.tree(object = model)
#plot(model.cv$size,model.cv$dev,'b') #Best (lowest deviance) value is 3. See plot.
model.pruned <- prune.tree(model,best=3)
plot(model.pruned)
text(model.pruned, cex=.75)
pred_data = predict(model.pruned,newdata = reorder_data)

plot(reorder_data$MET,reorder_data$EX, ylab = "EX", xlab = "MET", main="1.2 Original and fitted data")
points(reorder_data$MET, pred_data, col="blue")
legend("top", lty=c(1,1), col=c("black","blue"), legend = c("Original", "Fitted"))

residuals = reorder_data$EX-pred_data
hist(residuals)

#1.3

# computing bootstrap samples
bootstrap=function(data, ind){
  boot_data=data[ind,]# extract bootstrap sample
  res=tree(EX~MET, data=boot_data, 
           control = tree.control(minsize = 8, nobs = nrow(reorder_data)))
  #predict values for all Area values from the original
  model.pruned = prune.tree(res,best=3)
  EX_pred=predict(model.pruned,newdata=reorder_data)
  return(EX_pred)
}
res=boot(reorder_data, bootstrap, R=1000) #make bootstrap
e = envelope(res, level=0.95)

plot(reorder_data$MET, reorder_data$EX, pch=21, bg="orange", main="Non-parametric bootstrap", xlab="MET", ylab="EX")
legend("top", lty=c(1,1,1), col=c("orange","black", "blue"), legend = c("Data", "Model", "Confidence bands"))
points(reorder_data$MET,pred_data,type="l") #plot fitted line
#plot cofidence bands
points(reorder_data$MET,e$point[2,], type="l", col="blue")
points(reorder_data$MET,e$point[1,], type="l", col="blue")
#bumpy, tries to fit every data point
#reliable, inside the confidence bands

#1.4
mle=tree(reorder_data$EX~reorder_data$MET, data=reorder_data, 
         control = tree.control(minsize = 8, nobs = nrow(reorder_data)))
mle.pruned=prune.tree(mle,best=3)
rng=function(data, mle) {
  data1=data.frame(EX=reorder_data$EX,
                   MET=reorder_data$MET)
  n=length(data$EX)
  print(data1)
  #generate new EX
  p = predict(mle,newdata = data1)
  residuals = data1$EX-p
  data1$EX=rnorm(n,p,sd(residuals))
  return(data1)
}
f1=function(data1){
  #print(data1)
  res=tree(EX~MET, data=data1, 
           control = tree.control(minsize = 8, nobs = nrow(reorder_data)))
  #predict values for all Area values from the original data
  model.pruned = prune.tree(res,best=3)
  EX_pred=predict(model.pruned,newdata=reorder_data)
  return(EX_pred)
}
res_parametric=boot(reorder_data, statistic=f1, R=1000,
         mle=mle.pruned,ran.gen=rng, sim="parametric")
e_parametric = envelope(res_parametric, level=0.95)
plot(reorder_data$MET, reorder_data$EX, pch=21, bg="orange", main="Bootstrap comparison", xlab = "MET", ylab = "EX",ylim=c(100,600))
legend("top", lty=c(1,1), col=c("pink", "black", "blue","red"), legend = c("Prediction bands", "Predictions", "Non-parametric confidence bands", "Parametric confidence bands"))

points(reorder_data$MET,pred_data,type="l") #plot fitted line

#plot cofidence bands
points(reorder_data$MET,e_parametric$point[2,], type="l", col="red")
points(reorder_data$MET,e_parametric$point[1,], type="l", col="red")
points(reorder_data$MET,e$point[2,], type="l", col="blue")
points(reorder_data$MET,e$point[1,], type="l", col="blue")

#Prediction boundary
bootstrap.prediction <- function(data)
{
  model = tree(EX ~ MET, data=data, control = tree.control(nobs = nrow(data), minsize = 8))
  model.pruned = prune.tree(model,best=3)
  EX_pred = predict(model.pruned,data)
  n = length(data$EX)
  ndata = rnorm(n , EX_pred,sd(resid(model.pruned)))
  return(ndata)    
}

bootstrap3 = boot(reorder_data, bootstrap.prediction,R=1500, mle=mle.pruned, ran.gen=rng, sim="parametric")
e_pred_bands = envelope(bootstrap3,level=0.95)

points(reorder_data$MET, e_pred_bands$point[2,], type="l", col="pink")
points(reorder_data$MET, e_pred_bands$point[1,], type="l", col="pink")