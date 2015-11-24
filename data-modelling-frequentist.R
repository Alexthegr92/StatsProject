source("~/Documents/MAS8381/StatsProjects/StatsProject/data-tidying.R")

# linear regression model with all variables
mod1 = lm(Income~. , data=marketing)
summary(mod1)

# look at what anova does lol
anova1 = aov(Income~., data=marketing)
summary(anova1)

