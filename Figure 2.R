###############################
##VULNERABILITY CLAMS GALICIA##
##ELENA OJEA###################
##JULY 2018####################

##libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tibble)
library(scales)
library(dplyr)


##########################################################################################

## A: Vulnerativuty index plotting


##Read final Vulnerability index and ecological vulnerability

data <- read.csv("data_vul/Vulnerability.csv", sep=";")                 #open .csv vul index
data <- data[1:6,]                                                      #delete last three rows
data_EV <- read.csv("data_vul/Ecological vulnerability.csv", sep=";")   #open .csv ecological vulnerability
data_EV <- data_EV[1:6,]                                                #delete last three rows


##Join data on ecological vulnerability

data$guild <- as.character(data$guild)                                  #to convert column name "guild" into  a character
data_EV$guild <- as.character(data_EV$guild)                            #to convert column name "guild" into  a character
data_EV$guild[data_EV$guild == "Moana "] <-"Moaña"                      #to correct Moaña    to be common among data files 
data$guild[data$guild == "Moana "] <-"Moaña"                            #to correct Moaña    to be common among data files 


data <- left_join(data, data_EV, by="guild")                            #to combine data and data_EV by the joint column "guild"

##Join data on social sensitivity 
#Load data, social sensitivity (SS) and social impact potential (SIP)
dataSS <- read.csv("data_vul/social sensitivity.csv", sep =";")
dataSIP <- read.csv("data_vul/social impact potential.csv", sep = ";")

dataSS <- dataSS[1:6,] 
dataSS$guild <- as.character(dataSS$guild)                            #to convert column name "guild" into  a character
dataSS$guild[dataSS$guild == "Moana "] <-"Moaña" 
data <- left_join(data, dataSS, by="guild")

dataSIP <- dataSIP[1:6,]
dataSIP$guild <- as.character(dataSIP$guild)
dataSIP$guild[dataSIP$guild == "Moana "] <-"Moaña"
data <- left_join(data, dataSIP, by="guild")

##Figure 1:  SAC - EV - V

data$guild <- as.factor(data$guild)                              #convert guild in a factor so that I can label later

f1 <- ggplot(data, aes(EV,SAC, label = guild)) +                #ggplot for figure 1, using data 
  geom_jitter(alpha=0.8, aes(color=V),
              position = position_jitter(width = .001), size=10)+
  geom_text_repel(data=data, size=3.5,hjust=1.1)+
  xlab("Ecological vulnerability")+
  ylab("Social Adaptive Capacity")+
  theme(legend.position="right")+
  labs(colour = "Vulnerability score")+
  scale_color_gradient(low = "skyblue2", high = "skyblue4")+
  theme_minimal()+
  ylim(0,1)+ 
  xlim(-0.1,1.5)
f1
##Saving the figure in /Figures:

ggsave("Figures/Figure 1.png", dpi = 600)

##Figure 2: SAC - SS - V
f2 <- ggplot(data, aes(SAC,SS, label = guild)) +                #ggplot for figure 1, using data 
  geom_jitter(alpha=0.8, aes(color=V),
              position = position_jitter(width = .001), size=10)+
  geom_text_repel(data=data, size=3.5,hjust=1.1)+
  xlab("Social Adaptive Capacity")+
  ylab("Social Sensitivity")+
  theme(legend.position="right")+
  labs(colour = "Vulnerability score")+
  scale_color_gradient(low = "yellow", high = "red")+
  theme_minimal()+
  ylim(-0.1,1.5)+
  xlim(0,1)
f2
##Saving the figure in /Figures:

ggsave("Figures/Figure 2.png", dpi = 600)

##Figure 3: SAC - SIP - V
f3 <- ggplot(data, aes(SAC,SIP, label = guild)) +                #ggplot for figure 1, using data 
  geom_jitter(alpha=0.8, aes(color=V),
              position = position_jitter(width = .001), size=10)+
  geom_text_repel(data=data, size=3.5,hjust=1.1)+
  xlab("Social Adaptive Capacity")+
  ylab("Social impact potential")+
  theme(legend.position="right")+
  labs(colour = "Vulnerability score")+
  scale_color_gradient(low = "green", high = "dark green")+
  theme_minimal()+
  ylim(-0.1,1.5)+
  xlim(0,1)
f3
##Saving the figure in /Figures:

ggsave("Figures/Figure 3.png", dpi = 600)

##Saving the figure in /Figures:

ggsave("Figures/Figure 1.png", dpi = 600)

#Barplot, SAC

f4 <- ggplot(data = data, aes(x = guild, y = SAC))+
               geom_bar(stat = "identity", width = 0.5, fill = "black")+
  theme_minimal()
f4
##Saving the figure in /Figures:

ggsave("Figures/Figure 4.png", dpi = 600)



###################################################################################

## B: Create figure that compares indicators about SAC 


##Figure 5 Adaptive Capacity indicators

#open csv indicators

data_SAC <- read.csv("data_vul/SAC indicators.csv", sep=";")                 #open .csv SAC indicators

##add the SAC score
data_SAC$guild <- as.character(data_SAC$guild)                            #to convert column name "guild" into  a character
data_SAC$guild[data_SAC$guild == "Moana "] <-"Moaña"                      #to correct Moaña    to be common among data files 
data_SAC <- data_SAC[1:6,]                                                #to delete average and min and max values

data$guild <- as.character(data$guild)                                     #to convert guild from factor to character in data to merge
data_SAC <- left_join(data_SAC, data, by="guild")                            #to combine data_SAC and data by the joint column "guild"

#need to convert wide data into long format for ggplot:
data_long <- gather(data_SAC, indicator, value, experience:monoculture, factor_key=TRUE)
data_long


data_long <- data_long %>%
  group_by(indicator) %>%
  mutate(suma = sum(value))
data_long$suma <- as.factor(data_long$suma)
data_long$suma <- as.numeric(data_long$suma)

#how to order variables for ggplot? g <- ggplot(x, aes(reorder(variable, value), value))

f5 <-  ggplot(data_long, aes(reorder(indicator, suma), value, fill = guild)) + 
  geom_bar(stat = "identity", position = "stack", width=0.7)+
  #geom_text(aes(label = value), position = position_dodge(width = .8), size=3, vjust = -0.5)+
  xlab(NULL)+
  ylab(NULL)+
  #ylim(0,30)+
  scale_fill_brewer(palette="Spectral")+
  theme_bw()+
  coord_flip()+
  facet_wrap(~guild, scales="free_y") +
  scale_fill_discrete(guide=FALSE)
f5


##Saving the figure in /Figures:

ggsave("Figures/Figure 5.png", dpi = 600)
