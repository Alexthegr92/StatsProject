#simulating some data
n=500
p=20
X=matrix(rnorm(n*p),ncol=p)
beta=2^(0:(1-p))
print(beta)
alpha=3
tau=2
eps=rnorm(n,0,1/sqrt(tau))
y=alpha+as.vector(X%*%beta + eps)

#fitting the model by least squares
mod=lm(y~X)
print(summary(mod))

#fitting the saturated model using JAGS
require(rjags)
data=list(y=y,X=X,n=n,p=p)
init=list(tau=1,alpha=0,beta=rep(0,p))
modelstring="
  model {
    for (i in 1:n) {
      mean[i]<-alpha+inprod(X[i,],beta)
      y[i]~dnorm(mean[i],tau)
    }
    for (j in 1:p) {
      beta[j]~dnorm(0,0.001)
    }
    alpha~dnorm(0,0.0001)
    tau~dgamma(1,0.001)
  }
"
model=jags.model(textConnection(modelstring),
                 data=data,inits=init)
update(model,n.iter=100)
output=coda.samples(model=model,variable.names=c("alpha","beta","tau"),
                    n.iter=10000,thin=1)
print(summary(output))
plot(output)

#basic variable selection - kuo and mallick
data=list(y=y,X=X,n=n,p=p)
init=list(tau=1,alpha=0,betaT=rep(0,p),ind=rep(0,p))
modelstring="
  model {
    for (i in 1:n) {
      mean[i]<-alpha+inprod(X[i,],beta)
      y[i]~dnorm(mean[i],tau)
    }
    for (j in 1:p) {
      ind[j]~dbern(0.2)
      betaT[j]~dnorm(0,0.001)
      beta[j]<-ind[j]*betaT[j]
    }
    alpha~dnorm(0,0.0001)
    tau~dgamma(1,0.001)
  }
"
model=jags.model(textConnection(modelstring),
                 data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,
                    variable.names=c("alpha","beta","ind","tau"),
                    n.iter=10000,thin=1)
print(summary(output))
plot(output)

#variable selection with random effects
data=list(y=y,X=X,n=n,p=p)
init=list(tau=1,taub=1,alpha=0,betaT=rep(0,p),ind=rep(0,p))
modelstring="
  model {
    for (i in 1:n) {
      mean[i]<-alpha+inprod(X[i,],beta)
      y[i]~dnorm(mean[i],tau)
    }
    for (j in 1:p) {
      ind[j]~dbern(0.2)
      betaT[j]~dnorm(0,taub)
      beta[j]<-ind[j]*betaT[j]
    }
    alpha~dnorm(0,0.0001)
    tau~dgamma(1,0.001)
    taub~dgamma(1,0.001)
  }
"
model=jags.model(textConnection(modelstring),
                 data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,
                    variable.names=c("alpha","beta","ind","tau","taub"),
                    n.iter=10000,thin=1)
print(summary(output))
plot(output)

#variable selection with random effects and a prior on the inclusion probability
data=list(y=y,X=X,n=n,p=p)
init=list(tau=1,taub=1,pind=0.5,alpha=0,betaT=rep(0,p),ind=rep(0,p))
modelstring="
  model {
    for (i in 1:n) {
      mean[i]<-alpha+inprod(X[i,],beta)
      y[i]~dnorm(mean[i],tau)
    }
    for (j in 1:p) {
      ind[j]~dbern(pind)
      betaT[j]~dnorm(0,taub)
      beta[j]<-ind[j]*betaT[j]
    }
    alpha~dnorm(0,0.0001)
    tau~dgamma(1,0.001)
    taub~dgamma(1,0.001)
    pind~dbeta(2,8)
  }
"
model=jags.model(textConnection(modelstring),
                 data=data,inits=init)
update(model,n.iter=1000)
output=coda.samples(model=model,
                    variable.names=c("alpha","beta","ind","tau","taub","pind"),
                    n.iter=10000,thin=1)
print(summary(output))
plot(output)
