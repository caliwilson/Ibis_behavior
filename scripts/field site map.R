##Ibis field data map

library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(readxl)
library("maps")
library("tools")
library(lwgeom)
library("ggspatial")
library(osmdata)
library(ggmap)

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

sites <- read.csv(file = 'data/site_info.csv')
head(sites)
sites$lat<- as.numeric(sites$lat)
sites$long<- as.numeric(sites$long)

sites0<-sites

theme_set(theme_bw())


ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = long, y = lat, fill=site_type), size = 4, 
             shape = 23) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


sites <- st_as_sf(sites, coords = c("long", "lat"), 
                   crs = 4326, agr = "constant") #convert data frame to SF object

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = sites, aes(fill=site_type), size = 4, shape = 23) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)


#something breaks w this chunk:
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
head(states)
sf::sf_use_s2(FALSE) 
states <- cbind(states, st_coordinates(st_centroid(states)))
states$ID <- toTitleCase(states$ID)
head(states)

ggplot(data = world) +
  geom_sf() +
  geom_sf(data = states, fill = NA) + 
  geom_text(data = states, aes(X, Y, label = ID), size = 5) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

counties <- st_as_sf(map("county", plot = FALSE, fill = TRUE))
counties$pbc<-ifelse(counties$ID=='florida,palm beach',"yes","no")
counties <- subset(counties, grepl("florida", counties$ID))
counties$area <- as.numeric(st_area(counties))
head(counties)

#map of FL counties
ggplot(data = world) +
  geom_sf() +
  geom_sf(data = counties, fill = NA, color = gray(.5)) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE)

#Map of FL counties with some colors for field sites
ggplot(data = world) +
  geom_sf(fill = "antiquewhite1") +
  geom_sf(data = counties,aes(color = pbc)) +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = sites,aes(color=site_type), size = 4, shape = 23) +
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  scale_fill_viridis_c(trans = "sqrt", alpha = .4) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Observation Sites") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))



ggplot(data = world) +
  geom_sf(fill = "beige") +
  geom_sf(data = counties,aes(fill=pbc),color="black") +
  geom_sf(data = states, fill = NA) + 
  geom_sf(data = sites, size = 4, shape = 23,fill = "white") +
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  scale_fill_manual(values = c("#CCCCCC", "darkred"))+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Observation Sites") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))

#map highlighting Palm Beach County 
ggplot(data = world) +
  geom_sf(fill = "beige") +
  geom_sf(data = counties,aes(fill=pbc),color="black") +
  geom_sf(data = states, fill = NA)  +
  geom_label(data = states, aes(X, Y, label = ID), size = 5, fontface = "bold", 
             nudge_y = states$nudge_y) +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  coord_sf(xlim = c(-88, -78), ylim = c(24.5, 33), expand = FALSE) +
  scale_fill_manual(values = c("#CCCCCC", "#287D8EFF"))+
  xlab("Longitude") + ylab("Latitude") +
  theme(panel.grid.major = element_line(color = gray(0.5), linetype = "dashed", 
                                        size = 0.5), panel.background = element_rect(fill = "aliceblue"))+ theme(legend.position = "none") 

#trying to make a zoomed in map:
pbc_bb <- getbb(place_name = "Palm Beach County, Florida")
pbc_bb


pbc_bb %>%
  opq()

pbc_map <- get_map(pbc_bb, maptype = "roadmap")

ggmap(pbc_map) #+geom_sf(data = sites, size = 4, shape = 23,fill = "white") #fancy map of PBC

pbc_map2 <- get_map(pbc_bb, maptype = "terrain")

#zoomed in map with colors and colored site indicators 
ggmap(pbc_map2)  + 
  geom_point(data = sites0, aes(x = long, y = lat,fill=site_type),shape=23,size=6)+ 
  scale_fill_manual(values = c("#1F968BFF", "#481567FF"))+
  labs(fill="Site Type")+
  xlab("Longitude") + ylab("Latitude") + theme_bw()

#black and white map with colored site indicators 
pbc_map3 <- get_stamenmap(pbc_bb, maptype = "toner-lite")
ggmap(pbc_map3)  + 
  geom_point(data = sites0, aes(x = long, y = lat,fill=site_type),shape=23,size=6)+ 
  scale_fill_manual(values = c("#55C667FF", "#FDE725FF"))+
  labs(fill="Site Type")+
  xlab("Longitude") + ylab("Latitude") + theme_bw()

