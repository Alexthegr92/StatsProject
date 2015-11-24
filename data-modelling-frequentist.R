source("~/Documents/MAS8381/StatsProjects/StatsProject/data-tidying.R")

# linear regression model with all variables
mod1 = lm(Income~. , data=marketing)
summary(mod1)

# Best subset regression
bestSubsetFull = regsubsets(Income~., data=marketing, nvmax=20)
bestSubsetSummary = summary(bestSubsetFull)
bestSubsetSummary

par(mfrow=c(2,2))
plot(bestSubsetSummary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(bestSubsetSummary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type="l")
which.max(bestSubsetSummary$adjr2)
points(13,bestSubsetSummary$adjr2[13], col="red",cex=2,pch=20)
plot(bestSubsetSummary$cp,xlab="Number of Variables",ylab="Cp",type='l')
which.min(bestSubsetSummary$cp)
points(13,bestSubsetSummary$cp[13],col="red",cex=2,pch=20)
plot(bestSubsetSummary$bic,xlab="Number of Variables",ylab="BIC",type='l')
which.min(bestSubsetSummary$bic)
points(11,bestSubsetSummary$bic[11],col="red",cex=2,pch=20)
par(mfrow=c(1,1))
# doesnt reduce the model at all! use another method

# Lasso
marketing = marketingFactors
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
lasso.coef=predict(out,type="coefficients",s=bestlam)[1:14,]
lasso.coef #shows which are 0

lasso.coef[lasso.coef!=0]
lasso.coef[lasso.coef <0.010]
lasso.coef[lasso.coef>-0.01]
