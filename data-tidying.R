source("~/Documents/MAS8381/StatsProjects/StatsProject/packages.R")
data("marketing")

# change non ordered categorical varibales to factors
# leave the others but maybe convert to continuous vars after looking at the data plots etc

# omitted NAs
marketing = na.omit(marketing)

# make factors
marketing$Sex = factor(marketing$Sex)
marketing$Marital = factor(marketing$Marital)
marketing$Occupation = factor(marketing$Occupation)
marketing$Dual_Income = factor(marketing$Dual_Income)
marketing$Status = factor(marketing$Status)
marketing$Home_Type = factor(marketing$Home_Type)
marketing$Ethnic = factor(marketing$Ethnic)
marketing$Language = factor(marketing$Language)

