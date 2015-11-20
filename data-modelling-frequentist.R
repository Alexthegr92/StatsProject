# linear regression model with all variables
mod1 = lm(Income~. , data=marketing)
summary(mod1)

# look at what anova does lol
anova1 = aov(Income~., data=marketing)
summary(anova1)

plot(marketing$Sex, marketing$Income, main="sex")
plot(marketing$Marital, marketing$Income, main="marital")
plot(marketing$Occupation, marketing$Income, main="occupation")
plot(marketing$Dual_Income, marketing$Income, main="dual income")
plot(marketing$Status, marketing$Income, main="status")
plot(marketing$Home_Type, marketing$Income, main="home type")
plot(marketing$Ethnic, marketing$Income, main="ethnic")
plot(marketing$Language, marketing$Income, main="language")

