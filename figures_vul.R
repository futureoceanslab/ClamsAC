###############################
##VULNERABILITY CLAMS GALICIA##
##ELENA OJEA###################
##JULY 2018####################

##libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)

##Read final Vulnerability index and ecological vulnerability

data <- read.csv("data_vul/Vulnerability.csv", sep=";")                 #open .csv vul index
data <- data[1:6,]                                                      #delete last three rows
data_EV <- read.csv("data_vul/Ecological vulnerability.csv", sep=";")   #open .csv ecological vulnerability
data_EV <- data_EV[1:6,]                                                #delete last three rows


##Join data on ecological vulnerability

data$guild <- as.character(data$guild)                                  #to convert column name "guild" into  a character
data_EV$guild <- as.character(data_EV$guild)                            #to convert column name "guild" into  a character
data_EV$guild[data_EV$guild == "Moana "] <-"Moaña"                      #to correct Moaña    to be common among data files 
data$guild[data$guild == "Moaña "] <-"Moaña"                            #to correct Moaña    to be common among data files 

data <- left_join(data, data_EV, by="guild")                            #to combine data and data_EV by the joint column "guild"

##Join data on social sensitivity 

data_SS$guild <- as.character(data_SS$guild)                            #to convert column name "guild" into  a character


##Figure SAC - SIP - EV

data$guild <- as.factor(data$guild)
f1 <- ggplot(data, aes(SAC,SIP, label = guild)) +
  geom_jitter(alpha=0.8, aes(color=guild, size=EV),
              position = position_jitter(width = .001))+
  geom_text_repel(data=data, size=3, vjust=1)+
  #geom_text_repel(data=subset(latitude, latitude$b_value>100), size=3, vjust=1)+
  #ggtitle("")+
  xlab("Social Adaptive Capacity")+
  ylab("Social Impact Potential")+
  theme(legend.position="right")+
  theme_minimal()+
  ylim(0,1.5)+
  xlim(0,1)
f1


f2 <- ggplot(data, aes(SAC,EV, label = guild)) +
  geom_jitter(alpha=0.8, aes(color=guild, size=SIP),
              position = position_jitter(width = .001))+
  geom_text_repel(data=data, size=3, vjust=1)+
  #geom_text_repel(data=subset(latitude, latitude$b_value>100), size=3, vjust=1)+
  #ggtitle("")+
  xlab("Social Adaptive Capacity")+
  ylab("Ecological vulnerability")+
  theme(legend.position="right")+
  theme_minimal()+
  ylim(0,1.5)+
  xlim(0,1)
f2



