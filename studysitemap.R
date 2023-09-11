#making my map using Claire's example:

library(ggmap)
library(tidyverse)
library(gridExtra)

library(cowplot)

#Load site data
sites <- read.csv(file = 'data/site_info.csv')
head(sites)
sites$lat<- as.numeric(sites$lat)
sites$long<- as.numeric(sites$long)

#get google map
register_google(key = "AIzaSyDYum8S7ksfY0xiKJAsBmYHfBExxNP2jYk") #my code


#zoomed in field site map
p.terrain <- ggmap(get_googlemap(center = c(lon =  -80.242223, lat =26.692841),
                         zoom = 10, scale = 2,
                         maptype ='terrain',
                         color = 'color')) +
  geom_point(aes(x = long, y = lat,  fill = site_type), color="black",shape=25,size=5, data = sites) + 
  theme(legend.position="right") +
  xlab("Longitude") + ylab("Latitude")+scale_fill_manual(values=c("#21918c","#440154"), 
                                                         name="Site Type",
                                                         breaks=c("natural", "urban"),
                                                         labels=c("Natural", "Urban"))+
  theme(legend.key=element_rect(fill="white"))
p.terrain

ggsave("outputs/p.terrain.png")


p.satellite <- ggmap(get_googlemap(center = c(lon =  -80.242223, lat =26.692841),
                                 zoom = 10, scale = 2,
                                 maptype ='satellite',
                                 color = 'color')) +
  geom_point(aes(x = long, y = lat,  fill = site_type), color="black",shape=25,size=5, data = sites) + 
  theme(legend.position="right") +
  xlab("Longitude") + ylab("Latitude")+scale_fill_manual(values=c("#21918c","#440154"), 
                                                         name="Site Type",
                                                         breaks=c("natural", "urban"),
                                                         labels=c("Natural", "Urban"))+
  theme(legend.key=element_rect(fill="white"))
p.satellite

ggsave("outputs/p.satellite.png")

p.hybrid <- ggmap(get_googlemap(center = c(lon =  -80.242223, lat =26.692841),
                                   zoom = 10, scale = 2,
                                   maptype ='hybrid',
                                   color = 'color')) +
  geom_point(aes(x = long, y = lat,  fill = site_type), color="black",shape=25,size=5, data = sites) + 
  theme(legend.position="right") +
  xlab("Longitude") + ylab("Latitude")+scale_fill_manual(values=c("#21918c","#440154"), 
                                                         name="Site Type",
                                                         breaks=c("natural", "urban"),
                                                         labels=c("Natural", "Urban"))+
  theme(legend.key=element_rect(fill="white"))
p.hybrid 

ggsave("outputs/p.hybrid.png")

#get lat long limits for zoomed map to draw rectangle around big map:
p.hybrid$data[1,1] #xmin
p.hybrid$data[2,1] #xmax
p.hybrid$data[3,2] #ymin
p.hybrid$data[1,2] #ymax


#zoomed out FL map
p.hybrid_FL <- ggmap(get_googlemap(center = "Florida",
                                   zoom = 7, scale = 2,
                                   maptype ='hybrid',
                                   color = 'color')) +
  xlab("Longitude") + ylab("Latitude")+
  annotate(geom="rect", xmin = p.hybrid$data[1,1], xmax = p.hybrid$data[2,1], ymin = p.hybrid$data[3,2], 
                                                ymax = p.hybrid$data[1,2] , colour = "red", fill = NA,linewidth = 1.5)
  
p.hybrid_FL
ggsave("outputs/p.hybrid_FL.png")

p.terrain_FL <- ggmap(get_googlemap(center = "Florida",
                                   zoom = 7, scale = 2,
                                   maptype ='terrain',
                                   color = 'color')) +
  xlab("Longitude") + ylab("Latitude")+
  annotate(geom="rect", xmin = p.hybrid$data[1,1], xmax = p.hybrid$data[2,1], ymin = p.hybrid$data[3,2], 
           ymax = p.hybrid$data[1,2] , colour = "red", fill = NA,linewidth = 1.5)

p.terrain_FL
ggsave("outputs/p.terrain_FL.png")

p.satellite_FL <- ggmap(get_googlemap(center = "Florida",
                                    zoom = 7, scale = 2,
                                    maptype ='satellite',
                                    color = 'color')) +
  xlab("Longitude") + ylab("Latitude")+
  annotate(geom="rect", xmin = p.hybrid$data[1,1], xmax = p.hybrid$data[2,1], ymin = p.hybrid$data[3,2], 
           ymax = p.hybrid$data[1,2] , colour = "red", fill = NA,linewidth = 1.5)

p.satellite_FL
ggsave("outputs/p.satellite_FL.png")

grid.arrange(p.satellite_FL,p.terrain, ncol=2)

library(cowplot)
opt1<- plot_grid(p.satellite_FL,p.terrain,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt1.png")

opt2<- plot_grid(p.satellite_FL,p.satellite,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt2.png")

opt3<- plot_grid(p.satellite_FL,p.hybrid,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt3.png")

opt4<- plot_grid(p.terrain_FL,p.hybrid,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt4.png")

opt5<- plot_grid(p.terrain_FL,p.satellite,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt5.png")

opt6<- plot_grid(p.terrain_FL,p.terrain,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt6.png")

opt7<- plot_grid(p.hybrid_FL,p.terrain,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt7.png")

opt8<- plot_grid(p.hybrid_FL,p.satellite,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt8.png")

opt9<- plot_grid(p.hybrid_FL,p.hybrid,ncol=2, rel_widths = c(1/3, 2/3))
ggsave("outputs/opt9.png")


