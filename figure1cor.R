### This code is mean to plot socio-ecological dimensions from the cor set.
### Francesca Barazetta
### July 2018

# Load libraries
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(tidyr)
library(tibble)
library(scales)
library(dplyr)

# Load data
V <- read.csv("data_cor/Vulnerability.csv", sep=";")
EV <- read.csv("data_cor/Ecol_vul.csv", sep=";")
SAC <- read.csv("data_cor/SAC.csv", sep=";")
SIP <- read.csv("data_cor/SIP.csv", sep=";")
SS <- read.csv("data_cor/SS.csv", sep=";")

# Make guild column into character 
V$guild <- as.character(V$guild) 
EV$guild <- as.character(EV$guild)  
SAC$guild <- as.character(SAC$guild)
SIP$guild <- as.character(SIP$guild)
SS$guild <- as.character(SS$guild)

# Join datasets
data <- left_join(V, EV, by="guild")
data <- left_join(data, SAC, by="guild")
data <- left_join(data, SIP, by="guild")
data <- left_join(data, SS, by="guild")

# Make guild into factor
data$guild <- as.factor(data$guild) 

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

