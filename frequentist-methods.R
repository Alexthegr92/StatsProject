# Chapter 6 Lab 1: Subset Selection Methods

# Best Subset Selection

library(ISLR)

names(Hitters)
dim(Hitters)

sum(is.na(Hitters$Salary)) #counts the number of missing values
Hitters=na.omit(Hitters) #removes the missing values

dim(Hitters) #new dimension
sum(is.na(Hitters)) # we now have no missing values

library(leaps)
regfit.full=regsubsets(Salary~.,Hitters) #performs best model selection (need leaps library)
summary(regfit.full) #asterixes indicate the variables we should include, default 8 vars

regfit.full=regsubsets(Salary~.,data=Hitters,nvmax=19) #nvmax chooses max number of vars to incude in model
reg.summary=summary(regfit.full)

names(reg.summary) # can examine the diff statistics to choose the best model
reg.summary$rsq #we see R^2 increases

par(mfrow=c(2,2)) #look at all the stats to see which model is best
plot(reg.summary$rss,xlab="Number of Variables",ylab="RSS",type="l") #type = l joins the dots with lines
plot(reg.summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(reg.summary$adjr2) #finds maximum
points(11,reg.summary$adjr2[11], col="red",cex=2,pch=20) #plots a point on the original graph
plot(reg.summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(reg.summary$cp)
points(10,reg.summary$cp[10],col="red",cex=2,pch=20)
which.min(reg.summary$bic)
plot(reg.summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(6,reg.summary$bic[6],col="red",cex=2,pch=20)

par(mfrow=c(1,1))
plot(regfit.full,scale="r2")
plot(regfit.full,scale="adjr2")
plot(regfit.full,scale="Cp")
plot(regfit.full,scale="bic")
coef(regfit.full,6) #the coeff estimates associated with the 6 var model

# Forward and Backward Stepwise Selection

regfit.fwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="forward") #forward selection
summary(regfit.fwd)
regfit.bwd=regsubsets(Salary~.,data=Hitters,nvmax=19,method="backward") #backward selection
summary(regfit.bwd)

coef(regfit.full,7) #all up to 7 are identical. changes when the number of vars gets large
coef(regfit.fwd,7)
coef(regfit.bwd,7)

# Choosing Among Models

set.seed(1) #settting the test and training data sets
train=sample(c(TRUE,FALSE), nrow(Hitters),rep=TRUE)
test=(!train)

regfit.best=regsubsets(Salary~.,data=Hitters[train,],nvmax=19) #model training data
test.mat=model.matrix(Salary~.,data=Hitters[test,]) #put test data models in a matrix

val.errors=rep(NA,19) #empty vector
for(i in 1:19){
  coefi=coef(regfit.best,id=i) #get coeffs for each model
  pred=test.mat[,names(coefi)]%*%coefi #calculate predictions using test matrix
  val.errors[i]=mean((Hitters$Salary[test]-pred)^2) #calculate mean square error
}
val.errors

which.min(val.errors) #best model has smallest error
coef(regfit.best,10) #coeffs of best model

#makes our own function since regsubsets doesnt have one
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(Salary~.,data=Hitters,nvmax=19) #repeat on full data set so have more accurate estimates
coef(regfit.best,10) #pick 10 coeff model

#pick best model size using CV
k=10
set.seed(1)
folds=sample(1:k,nrow(Hitters),replace=TRUE) #10 folds
cv.errors=matrix(NA,k,19, dimnames=list(NULL, paste(1:19))) #puts errors in a matrix

#calculates the errors
for(j in 1:k){
  best.fit=regsubsets(Salary~.,data=Hitters[folds!=j,],nvmax=19)
  for(i in 1:19){
    pred=predict(best.fit,Hitters[folds==j,],id=i)
    cv.errors[j,i]=mean( (Hitters$Salary[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean) #calculates the mean error of each column
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')
reg.best=regsubsets(Salary~.,data=Hitters, nvmax=19) #full data set model
coef(reg.best,11) #11 var model is selected as best from plot


# Chapter 6 Lab 2: Ridge Regression and the Lasso

x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

# Ridge Regression

library(glmnet)
grid=10^seq(10,-2,length=100)
ridge.mod=glmnet(x,y,alpha=0,lambda=grid) #alpha=0 so ridge regression

dim(coef(ridge.mod))

ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2)) #l2 norm

ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))

predict(ridge.mod,s=50,type="coefficients")[1:20,] #ridge coeffs for lambda = 50

set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid, thresh=1e-12)
ridge.pred=predict(ridge.mod,s=4,newx=x[test,])
mean((ridge.pred-y.test)^2)

mean((mean(y[train])-y.test)^2)

ridge.pred=predict(ridge.mod,s=1e10,newx=x[test,])
mean((ridge.pred-y.test)^2)

ridge.pred=predict(ridge.mod,s=0,newx=x[test,],exact=T)
mean((ridge.pred-y.test)^2)

lm(y~x, subset=train)
predict(ridge.mod,s=0,exact=T,type="coefficients")[1:20,]

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0) #10 fold cv to find best lambda
plot(cv.out)
bestlam=cv.out$lambda.min
bestlam

ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,]) #mse of this lambda
mean((ridge.pred-y.test)^2)

out=glmnet(x,y,alpha=0) #ridge regression coeffs
predict(out,type="coefficients",s=bestlam)[1:20,]

# The Lasso

lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1) #cv fir associated test error
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:20,]
lasso.coef #shows which are 0

lasso.coef[lasso.coef!=0] #want these coeffs in model

# Chapter 6 Lab 3: PCR and PLS Regression

# Principal Components Regression

library(pls)
set.seed(2)
pcr.fit=pcr(Salary~., data=Hitters,scale=TRUE,validation="CV")
summary(pcr.fit)
validationplot(pcr.fit,val.type="MSEP") #plots CV scores

set.seed(1)
pcr.fit=pcr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV") #PCR on training data
validationplot(pcr.fit,val.type="MSEP")
pcr.pred=predict(pcr.fit,x[test,],ncomp=7)
mean((pcr.pred-y.test)^2)
pcr.fit=pcr(y~x,scale=TRUE,ncomp=7)
summary(pcr.fit)

# Partial Least Squares

set.seed(1)
pls.fit=plsr(Salary~., data=Hitters,subset=train,scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit,val.type="MSEP")
pls.pred=predict(pls.fit,x[test,],ncomp=2)
mean((pls.pred-y.test)^2)
pls.fit=plsr(Salary~., data=Hitters,scale=TRUE,ncomp=2)
summary(pls.fit)
