### PRINCIPAL COMPONENT ANALYSIS (PCA) OF SAC INDICATORS ###
###FRANCESCA BARAZZETTA###
##JULY 2018###

#open csv indicators

data_SAC <- read.csv("data_cor/SAC_indicators.csv", sep=";")
rownames(data_SAC)<-data_SAC[,1]
data_SAC<-data_SAC[,-1]

log<-log(data_SAC)

pca<-prcomp(data_SAC,center = T, scale. = T)
print(pca)
plot(pca, type= "l")
summary(pca)


library(ggfortify)
autoplot(pca, data=data_SAC, label=T, colour ="cooperation") 
ggsave("Figures/Figpca_coop.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="formation")
ggsave("Figures/Figpca_form.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="job._experience") 
ggsave("Figures/Figpca_jobex.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="experience") 
ggsave("Figures/Figpca_exp.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="mark_influ")
ggsave("Figures/Figpca_markinf.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="monitoring") 
ggsave("Figures/Figpca_monit.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="isolation") 
ggsave("Figures/Figpca_iso.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="credits_div") 
ggsave("Figures/Figpca_credit.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="trust") 
ggsave("Figures/Figpca_trust.png", dpi = 600)
autoplot(pca, data=data_SAC, label=T, colour ="education")
ggsave("Figures/Figpca_edu.png", dpi = 600)
