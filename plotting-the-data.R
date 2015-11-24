source("~/Documents/MAS8381/StatsProjects/StatsProject/data-tidying.R")

# plotting factors against income
plot(marketing$Sex, marketing$Income, main="sex")
plot(marketing$Marital, marketing$Income, main="marital")
plot(marketing$Occupation, marketing$Income, main="occupation")
plot(marketing$Dual_Income, marketing$Income, main="dual income")
plot(marketing$Status, marketing$Income, main="status")
plot(marketing$Home_Type, marketing$Income, main="home type")
plot(marketing$Ethnic, marketing$Income, main="ethnic")
plot(marketing$Language, marketing$Income, main="language")

# plotting continuous variables against income
plot(marketing$Age, marketing$Income, main="Age")
# simply plotting the data doesnt tell us anything

# try plotting age as a histogram for each income level
for( i in 1:9) {
  hist(marketing$Age[marketing$Income==i], main=paste("Histogram of Age for Income level ",i))
}
# can now see much more clearly the distribution of income for each age group

#function for plotting continuous variables for each level of income
continuousPlots = function(variable, name) {
  for( i in 1:9) {
    hist(variable[marketing$Income==i], main=paste("Histogram of ", name , "for income level ",i),
         xlab=name, breaks = max(variable) + 2, xlim = c(1, max(variable)))
  }
}

continuousPlots(marketing$Age, "Age")
continuousPlots(marketing$Edu, "Edu")
continuousPlots(marketing$Lived, "Lived")
# doesnt seem significant in plots
continuousPlots(marketing$Household, "Household")
continuousPlots(marketing$Householdu18, "Householdu18")