###### ECO Indicators Correlations ######
#Read Ecological sensitivity
es <- read.table("data/ES.txt",sep="\t",h=T)
es <- es [, c(3:6)]

es_cor <- cor(es, use = "pairwise.complete.obs")
p1 <- corrplot(es_cor, order ="AOE")
N1 <- es_cor
write.table(N1, file = "data2/N1.txt") 
write.csv(es_cor, file = "data2/N1.csv", sep=" ")

#Read recovery potential
rp <- read.table("data/rp.txt",sep="\t",h=T)
rp <- es [, c(2:4)]
rp_cor <- cor(rp, use = "pairwise.complete.obs")
p2 <- corrplot(rp_cor, order ="AOE")
N2 <-rp_cor
write.csv(N2,file = "data2/N2.csv")

#Read SS indicators
ss <- read.table("data/ss.txt",sep="\t",h=T)
ss <-ss [, c(2:5)]

ss_cor <- cor(ss, use = "pairwise.complete.obs")
p3 <- corrplot(ss_cor, order ="AOE")
N3 <-ss_cor
write.csv(N3,file = "data2/N3.csv")

#Read SAC indicators
ac <- read.table("data/ac.txt",sep="\t",h=T)
ac <-ac [, c(2:13)]
ac_cor <- cor(ac, use = "pairwise.complete.obs")
p4 <- corrplot(ac_cor, order ="AOE")
N4 <-ac_cor
write.csv(N4,file = "data2/N4.csv")

