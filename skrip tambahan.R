
setwd("D:\\[2021] R Ladies\\SNA Star Wars")
ds1<-read.csv("ds.csv")
library(vtable)
library(ggplot2)
vtable(ds1)
devtools::install_github("butterflyology/spaceMovie")
library("spaceMovie")
library(RColorBrewer)
library(viridis)

ggplot(ds1) + 
  geom_segment(aes(x=episode_start,xend=episode_end, y=character, yend=character, color=character),size=10)+ 
  scale_color_manual(values = SW_palette("Boba", n = 19, type = "continuous"))+
  scale_x_discrete(name ="Episode", 
                   limits=c("1","2","3",
                            "4","5","6",
                            "7","8","9"
                            ),
                   labels=c("1" = "The Phantom Menace (1999)", "2" = "Attack of the Clones (2002)","3" = "Revenge of the Sith (2005)",
                            "4" = "A New Hope (1977)", "5" = "The Empire Strikes Back (1980)","6" = "Return of the Jedi (1983)",
                            "7" = "The Force Awakens (2015)", "8" = "The Last Jedi (2017)","9" = "The Rise of Skywalker (2019)"))+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  labs(y = "Character")

ggplot(ds1) + 
  geom_segment(aes(x=episode_start,xend=episode_end, y=character, yend=character, color=character),size=10)+ 
  scale_colour_viridis_d(option = "plasma")+
  scale_x_discrete(name ="Episode", 
                   limits=c("1","2","3",
                            "4","5","6",
                            "7","8","9"
                   ),
                   labels=c("1" = "The Phantom Menace (1999)", "2" = "Attack of the Clones (2002)","3" = "Revenge of the Sith (2005)",
                            "4" = "A New Hope (1977)", "5" = "The Empire Strikes Back (1980)","6" = "Return of the Jedi (1983)",
                            "7" = "The Force Awakens (2015)", "8" = "The Last Jedi (2017)","9" = "The Rise of Skywalker (2019)"))+
  theme_bw(base_size = 14)+
  theme(axis.text.x = element_text(angle = 20, hjust = 1))+
  labs(y = "Character")
