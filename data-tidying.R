source("~/Documents/MAS8381/StatsProjects/StatsProject/packages.R")
data("marketing")

# change non ordered categorical varibales to factors
# leave the others but maybe convert to continuous vars after looking at the data plots etc

# omitted NAs
marketing = na.omit(marketing)

# make factors
marketingFactors = marketing
marketingFactors$Sex = factor(marketing$Sex)
marketingFactors$Marital = factor(marketing$Marital)
marketingFactors$Occupation = factor(marketing$Occupation)
marketingFactors$Dual_Income = factor(marketing$Dual_Income)
marketingFactors$Status = factor(marketing$Status)
marketingFactors$Home_Type = factor(marketing$Home_Type)
marketingFactors$Ethnic = factor(marketing$Ethnic)
marketingFactors$Language = factor(marketing$Language)

