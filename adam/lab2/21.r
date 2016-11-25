setwd("~/code/skola/tdde01/adam/lab2")

#linear regression
mylin=function(X,Y, Xpred){
  Xpred1=cbind(1,Xpred)
  X1 = cbind(1,X)
  beta = solve((t(X1) %*% as.matrix(X1))) %*% (t(X1)%*%Y)
  Ypred1=Xpred1%*%beta
  return(Ypred1)
}

myCV=function(X,Y,Nfolds){
  n=length(Y)
  p=ncol(X)
  set.seed(12345)
  ind=sample(n,n)
  X1=X[ind,]            # Shuffle
  Y1=Y[ind]             # Shuffle
  sF=floor(n/Nfolds)    # Rows per folds
  MSE=numeric(2^p-1)    # Empty vector with length 31
  Nfeat=numeric(2^p-1)  # Empty vector with length 31
  Features=list()
  curr=0
  
  #we assume 5 features.
  feats = c("Fertility", "Agriculture", "Examination", "Education", "Catholic", "Infant.Mortality")
  for (f1 in 0:1)
    for (f2 in 0:1)
      for(f3 in 0:1)
        for(f4 in 0:1)
          for(f5 in 0:1){
            model= c(f1,f2,f3,f4,f5)
            if (sum(model)==0) next()
            SSE=0
            
            Xcol=X1
            for(i in 1:5) {
              if(model[i] == 0) {
                Xcol=Xcol[, !(colnames(Xcol) %in% c(feats[i+1])), drop = F]
              }
            }
            
            for (k in 1:Nfolds){
              #MISSING: compute which indices should belong to current fold
              start_i = k*9-8
              if(start_i==0) start_i=1
              end_i = k*9
              if(end_i>n) end_i=n
              
              x_test = Xcol[start_i:end_i,]
              y_test = Y1[start_i:end_i]

              x_train = Xcol[-(start_i:end_i),]
              y_train = Y1[-(start_i:end_i)]
              
			        #MISSING: implement cross-validation for model with features in "model" and iteration i.
              m = mylin(x_train, y_train, x_test)
              
			        #MISSING: Get the predicted values for fold 'k', Ypred, and the original values for fold 'k', Yp.
              Yp = y_test
              Ypred = m
              SSE=SSE+sum((Ypred-Yp)^2)
            }
            curr=curr+1
            MSE[curr]=SSE/n
            Nfeat[curr]=sum(model)
            Features[[curr]]=model
            
          }
  #MISSING: plot MSE against number of features
  for(i in 1:curr) {
    length_vector[i] = sum(Features[[i]])
  }

  plot(length_vector, MSE, xlab="# of features", ylab="CV score")
  
  i=which.min(MSE)
  return(list(CV=MSE[i], Features=Features[[i]]))
}

l = myCV(as.matrix(swiss[,2:6]), swiss[[1]], 5)

