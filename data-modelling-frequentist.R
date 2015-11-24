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

par(mfrow=c(2,2))

plot(bestSubsetSummary$rss,xlab="Number of Variables",ylab="RSS",type="l")

plot(bestSubsetSummary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(bestSubsetSummary$adjr2)
points(28,bestSubsetSummary$adjr2[28], col="red",cex=2,pch=20)

plot(bestSubsetSummary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(bestSubsetSummary$cp)
points(28,bestSubsetSummary$cp[28],col="red",cex=2,pch=20)

plot(bestSubsetSummary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(bestSubsetSummary$bic)
points(21,bestSubsetSummary$bic[11],col="red",cex=2,pch=20)
par(mfrow=c(1,1))

coef(bestSubsetFull,21)

#try k fold cv
k=10
set.seed(1)
folds=sample(1:k,nrow(marketing),replace=TRUE)
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
coef(reg.best,11)

# Lasso
x=model.matrix(Income~.,marketing)[,-1]
y=marketing$Income
set.seed(1)
train=sample(1:nrow(x), nrow(x)/2)
test=(-train)
y.test=y[test]
grid=10^seq(10,-2,length=100)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid)
plot(lasso.mod)

set.seed(1) #cv for associated test error
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda=grid)
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:noVars,]
lasso.coef

lasso.coef[lasso.coef!=0]
