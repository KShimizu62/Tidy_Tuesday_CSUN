library(galah) # API to ALA
library(lubridate)
library(tidyverse)
library(rnoaa)
library(here)
library(maps)
library(mapdata)
library(mapproj)

numbats <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-03-07/numbats.csv')

Wherethenumbatsare<- numbats%>%
  

australia<-map_data("world", region="Australia")

pikachu<- numbats


view(pikachu)

numbatplot <- ggplot()+
  geom_polygon(data = australia, 
               aes(x = long,
                   y = lat,
                   group = group, 
                   fill = region),
               color = "black")+
  coord_map()+
  theme_void() +
  geom_point(data = pikachu,
             aes(x = decimalLongitude,
                 y= decimalLatitude,
                 size="numbats location"),
             color="black") +
  theme(panel.background = element_rect(fill = "lightblue"))
  ggsave(ggsave(here("TT_02","output","Tidytuesday_02_plot.pdf")))
 
numbatplot
