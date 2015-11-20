# linear regression model with all variables
mod1 = lm(Income~. , data=marketing)
summary(mod1)

anova1 = aov(Income~., data=marketing)
summary(anova1)

plot(marketing$Sex, marketing$Income)
plot(marketing$Marital, marketing$Income)
