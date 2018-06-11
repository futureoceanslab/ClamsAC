###### ECO Indicators Normalization ######

eco_indicators <- fread("data/Eco indicators.csv")
eco_indicators <- as.factor(eco_indicators)

eco_indicators_cor <- cor(Eco_indicators, use = "pairwise.complete.obs")
p1 <- corrplot(Eco_indicators_cor, order ="AOE")