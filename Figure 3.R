##### 1. READ DATA #####
Eco_factors <- read_csv2("data/Eco factors.csv") 
Eco_factors <- Eco_factors [, c(2:5)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("CATCH_TREND","THREAT","HEALTH")))
SAC_factors <- read_csv2("data/SAC factors.csv")
SAC_factors <- SAC_factors [, c(2:7)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("HM_AGENCY","LVL_DVNT","EC_STRENGHT", "AD_MNGMT", "SC_CAPITAL")))
SS_factors <- read.csv2("data/SS factors.csv")
SS_factors <- SS_factors [, c(2:5)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("SEA_CONDITION","DIV_RESOURCE","DEPENDENCE")))

##### 2. PLOT #####


p1<-ggplot(data=na.omit(SAC_factors), aes(x= guild , y=VALUE, fill=(FACTOR)))+
  geom_bar(aes(fill = FACTOR),stat = "identity")+
  scale_fill_manual(values=c("seagreen4","seagreen4","seagreen4", "seagreen4", "seagreen4"))+
  facet_grid(~FACTOR)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=(8)), axis.text.y = element_text(size = 7, hjust = 1))+
  ylab("SAC Factors")
p1

p2<-ggplot(data=na.omit(Eco_factors), aes(x= guild , y=VALUE, fill=(FACTOR)))+
  geom_bar(aes(fill = FACTOR),stat = "identity")+
  scale_fill_manual(values=c("cornsilk3","cornsilk3","cornsilk3"))+
  facet_grid(~FACTOR)+
  coord_flip()+
  theme_classic()+
  theme(text = element_text(size=(8)), axis.text.y = element_text(size = 7, hjust = 1))+
  ylab("Eco Factors")
p2

p3<-ggplot(data=na.omit(SS_factors), aes(x= guild , y=VALUE, fill=FACTOR))+
  geom_bar(aes(fill = FACTOR),stat = "identity")+
  scale_fill_manual(values=c("yellow3","yellow3","yellow3"))+
  facet_grid(~FACTOR)+
  coord_flip()+
  theme_minimal()+
  theme(text = element_text(size=(8)))+
  ylab("SS FACTORS")
p3

grid_arrange_shared_legend(p3, p1, p2, nrow = 1, ncol =3)