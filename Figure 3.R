##### 1. READ DATA #####
Eco_factors <- read_csv2("data/Eco factors.csv") 
Eco_factors <- Eco_factors [, c(2:5)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("CATCH_TREND","THREAT","HEALTH")))
SAC_factors <- read_csv2("data/SAC factors.csv")
SAC_factors <- SAC_factors [, c(2:7)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("HM_AGENCY","LVL_DVNT","EC_STRENGHT", "AD_MNGMT", "SC_CAPITAL")))
SS_factors <- read.csv2("data/SS factors.csv")
SS_factors <- SS_factors [, c(2:5)]%>% gather(FACTOR, VALUE,-guild, factor_key=TRUE)  %>% mutate(FACTOR=gsub("\\."," ",as.character(FACTOR)))  %>% mutate(FACTOR=factor(FACTOR,levels = c("SEA_CONDITION","DIV_RESOURCE","DEPENDENCE")))

##### 2. PLOT to save manual #####


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
  theme_classic()+
  theme(text = element_text(size=(8)))+
  ylab("SS FACTORS")
p3

grid_arrange_shared_legend(p3, p1, p2, nrow = 1, ncol =3)
##save manual##


##### 3. PLOT Fig 3 #####
dimensions <- c("Eco_factors","SAC_factors","SS_factors")

dimension_colors <- list(eco_factors=c("seagreen3"),
                         soc_factors=c("yellow3"),
                         ins_factors=c("cornsilk3"))

graphs <- dimensions  %>% lapply(function(dimension){
  # dimension <- "soc_factors"
  x<- get(dimension)
  
  g <- ggplot(data=na.omit(x), aes(x= guild , y=VALUE, fill=FACTOR))+
    geom_bar(stat = "identity", position=position_dodge(width=0.4), width=0.3)+
    scale_fill_manual(values=c("cornsilk3","cornsilk3","cornsilk3", "cornsilk3", "cornsilk3"))+
    facet_grid(~FACTOR)+
    coord_flip()+
    theme_minimal()+
    ylab("")+
    xlab("COUNTRY")+
    theme(axis.text.y = element_text(size=20, color="black"),
          axis.text.x = element_text(size=16,color="black",angle=50,hjust=1),
          legend.text = element_text(size=22,color = "black"),
          legend.title = element_text(size=24,color="black"),
          panel.spacing.x = unit(1,"lines"),
          strip.text = element_text(color="black",face="bold",size=11))
  
  if(dimension!="eco_factors"){
    
    g <- g + theme(axis.title.y = element_blank())
  }else{
    g <- g + theme(axis.title.y = element_text(size=26, color="black"))
    
  }
  g 
})


png("Figures/Fig 3_2.png",width=25,height=12,units="in",res=300)

do.call(grid_arrange_shared_legend,c(graphs, list(nrow = 1, ncol = 3)))

dev.off()


