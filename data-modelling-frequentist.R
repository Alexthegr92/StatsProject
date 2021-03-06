#set working directory to be stats project before sourcing
source("./data-tidying.R")

# linear regression model with all variables
mod1 = lm(Income~. , data=marketing)
summary(mod1)

noVars = nlevels(marketing$Sex) + nlevels(marketing$Marital) + nlevels(marketing$Occupation) + nlevels(marketing$Dual_Income) +
  + nlevels(marketing$Status) + nlevels(marketing$Home_Type) + nlevels(marketing$Ethnic) + nlevels(marketing$Language) + 5 - 8

# Best subset regression
bestSubsetFull = regsubsets(Income~., data=marketing, nvmax=noVars)
bestSubsetSummary = summary(bestSubsetFull)
bestSubsetSummary

# look at the best models according to summary stats
par(mfrow=c(2,2))

plot(bestSubsetSummary$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(bestSubsetSummary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
maxAdjr2 = which.max(bestSubsetSummary$adjr2)
points(maxAdjr2,bestSubsetSummary$adjr2[maxAdjr2], col="red",cex=2,pch=20)

plot(bestSubsetSummary$cp,xlab="Number of Variables",ylab="Cp",type='l')
mincp = which.min(bestSubsetSummary$cp)
points(mincp,bestSubsetSummary$cp[mincp],col="red",cex=2,pch=20)

plot(bestSubsetSummary$bic,xlab="Number of Variables",ylab="BIC",type='l')
minbic = which.min(bestSubsetSummary$bic)
points(minbic,bestSubsetSummary$bic[minbic],col="red",cex=2,pch=20)
par(mfrow=c(1,1))

# initial coefficicients of best subset
coef(bestSubsetFull,minbic)

# use cross validation to find the best number of predictors

# create predict function for regsubsets
predict.regsubsets=function(object,newdata,id,...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form,newdata)
  coefi=coef(object,id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

# k fold cv - k=10
k=10
set.seed(1)
folds=sample(1:k,nrow(marketing),replace=TRUE)
cv.errors=matrix(NA,k,noVars, dimnames=list(NULL, paste(1:noVars)))

# calculate the errors associated with each variable in each model
for(j in 1:k){
  best.fit=regsubsets(Income~.,data=marketing[folds!=j,],nvmax=noVars)
  for(i in 1:noVars){
    pred=predict(best.fit,marketing[folds==j,],id=i)
    cv.errors[j,i]=mean( (marketing$Income[folds==j]-pred)^2)
  }
}

#calculate the mean error in each model
mean.cv.errors=apply(cv.errors,2,mean)

plot(mean.cv.errors,type='b')
mincverrors = which.min(mean.cv.errors)
points(mincverrors, mean.cv.errors[mincverrors],col="red",cex=2,pch=20)

# the final model's coefficients
reg.best=regsubsets(Income~.,data=marketing, nvmax=noVars)
coef(reg.best, mincverrors)

# Lasso
x=model.matrix(Income~.,marketing)[,-1]
y=marketing$Income

#splitting the data into test and training set
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]

#setting up the lasso model with a range of values for lambda
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

#using cv to find the best value of lambda
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min

#predicting the mean error with this value of lambda
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

#calculating the non-zero coeffs using the best lambda and all the data
out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:noVars,]
lasso.coef[lasso.coef!=0]

#number of variables to include in the model
reducedNoVarsLasso = length(lasso.coef[lasso.coef!=0])
#only reduces the number of variables to include in the data set by 2 - use best subset