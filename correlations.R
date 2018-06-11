###### ECO Indicators Correlations ######
#Read Ecological indicators
eco_indicators <- read.csv2("data/Eco indicators.csv")
eco_indicators <- eco_indicators [, c(3:8)]

eco_indicators_cor <- cor(eco_indicators, use = "pairwise.complete.obs")
p1 <- corrplot(eco_indicators_cor, order ="AOE")

#Read SAC indicators
SAC_indicators <- read.csv2("data/SAC indicators.csv")
SAC_indicators <- SAC_indicators [, c(3:14)]

SAC_indicators_cor <- cor(SAC_indicators, use = "pairwise.complete.obs")
p2 <- corrplot(SAC_indicators_cor, order ="AOE")

#Read SS indicators
x <- read.csv2("data/SS indicators.csv")
x <- x [, c(3:14)]

x <- cor(x, use = "pairwise.complete.obs")
p2 <- corrplot(x, order ="AOE")