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


##Figure 1:  SAC - EV - V

data$guild <- as.factor(data$guild)                              #convert guild in a factor so that I can label later

f1 <- ggplot(data, aes(SAC,EV, label = guild)) +                #ggplot for figure 1, using data 
  geom_jitter(alpha=0.8, aes(color=V),
              position = position_jitter(width = .001), size=10)+
  geom_text_repel(data=data, size=3.5,hjust=1.1)+
  xlab("Social Adaptive Capacity")+
  ylab("Ecological vulnerability")+
  theme(legend.position="right")+
  labs(colour = "Vulnerability score")+
  scale_color_gradient(low = "skyblue2", high = "skyblue4")+
  theme_minimal()+
  ylim(-0.1,1.5)+
  xlim(0,1)
f1


##Saving the figure in /Figures:

ggsave("Figures/Figure 1.png", dpi = 600)




