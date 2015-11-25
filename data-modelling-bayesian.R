#set working directory to be stats project before sourcing
source("./data-tidying.R")

design = model.matrix(Income~., marketing)[,-1]
response = marketing$Income
n=nrow(design)
p=ncol(design)
beta=2^(0:(1-p))

#fitting the saturated model using JAGS
data=list(y=response,X=design,n=n,p=p)
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

#variable selection with random effects
data=list(y=response,X=design,n=n,p=p)
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
autocorr.plot(output)