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


##Figure 2 Adaptive Capacity indicators

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

g <- ggplot(x, aes(reorder(variable, value), value))

f2 <-  ggplot(data_long, aes(reorder(indicator, suma), value, fill = guild)) + 
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
f2


##Saving the figure in /Figures:

ggsave("Figures/Figure 2.png", dpi = 600)

