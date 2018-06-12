##### 1. READ DATA #####

# Read the data

AC_index <- read_csv2("data/AC index.csv")
AC_index <- AC_index [, c(2:3)]

##### 2. PLOT #####

ggplot(AC_index, aes(x=guild, y=AC))+
  geom_bar(stat="identity")+
  coord_flip()+
  theme_classic()+
  theme(legend.position = "none",
        strip.text = element_text(size=18,color="black",face="bold",hjust=0),
        strip.background = element_blank(),
        axis.text = element_text(size=16, color="black"),
        axis.title = element_text(size=20, color="black"),
        legend.text = element_text(size=22,color = "black"),
        legend.title = element_text(size=24,color="black"))

# Save

ggsave("Figures/Fig 2.png",width = 10,height = 8,units = "in")