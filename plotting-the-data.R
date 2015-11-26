source("./data-tidying.R")

# plotting factors against income
plot(marketing$Sex, marketing$Income, xlab="sex", ylab="Income")
plot(marketing$Marital, marketing$Income, xlab="marital", ylab="Income")
plot(marketing$Occupation, marketing$Income, xlab="occupation", ylab="Income")
plot(marketing$Dual_Income, marketing$Income, xlab="dual income", ylab="Income")
plot(marketing$Status, marketing$Income, xlab="status", ylab="Income")
plot(marketing$Home_Type, marketing$Income, xlab="home type", ylab="Income")
plot(marketing$Ethnic, marketing$Income, xlab="ethnic", ylab="Income")
plot(marketing$Language, marketing$Income, xlab="language", ylab="Income")

# plotting continuous variables against income
#plot(marketing$Age, marketing$Income, main="Age")
# simply plotting the data doesnt tell us anything

# try plotting age as a histogram for each income level
for( i in 1:9) {
  hist(marketing$Age[marketing$Income==i], main=paste("Histogram of Age for Income level ",i))
}
#can now see much more clearly the distribution of income for each age group

#function for plotting continuous variables for each level of income
continuousPlots = function(variable, name) {
  par(mfrow=c(1,2), xpd=NA)
  for( i in 1:9) {
    hist(variable[marketing$Income==i], xlab=name, breaks = seq(min(variable), 
        max(variable), by=0.5), xlim = c(min(variable), max(variable)), border=i, ylim=c(0,1.7), 
        freq=FALSE, main=NULL)
    par(new=TRUE)
  }
  legend(max(variable)+0.2,1.5, paste(1:9), lty=1, col=1:9, ncol=1,title = "Income level")
  par(mfrow=c(1,1), new=FALSE)
}

continuousPlots(marketing$Age, "Age")
continuousPlots(marketing$Edu, "Edu")
continuousPlots(marketing$Lived, "Lived")
# doesnt seem significant in plots
continuousPlots(marketing$Household, "Household")
continuousPlots(marketing$Householdu18, "Householdu18")
