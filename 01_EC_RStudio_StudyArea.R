getwd()
setwd("C:/Users/Doctor Shandu/OneDrive - University of Witwatersrand/MODULES/STAT 7006A_Spatial Stats/Project/Data/R_Stations/Work")

install.packages("rlang")
remove.packages("")

#Library
library(sf)
library(ggplot2)
library(mapview)
library(data.table)
library(rlang)
library(maptools)
library(ggspatial)
library(sp)

###########################################################################
#Loading data
ec_district <- st_read("./Shapefile/ec_district.shp")
st_crs(ec_district)
st_bbox(ec_district)
crs(ec_district)
ec_district 

ec_localm <- st_read("./Shapefile/ec_localMun.shp")
ec_localm
ec_stations <- read.csv("Precipitation.csv")


ec_stationSimpleLat <- read.csv("Precipitation.csv")

#ec_stationSimpleLat = as.data.table(ec_stationSimpleLat) #Creating data table
#############################################################################
#Projection

#Projection Coordinate System
projection(ec_district)
st_crs(ec_district)

coordinates(ec_stationSimpleLat) = c("Longitude", "Latitude")
crs.geo1 = CRS("+proj=longlat +datum=WGS84 +no_defs")
proj4string(ec_stationSimpleLat) = crs.geo1
View(ec_stationSimpleLat)
###########################################################################
#Ploting 2 shapefiles
library("ggrepel")

#basic <- ggplot() + 
  #geom_sf(data = ec_district, size = 2, color = "black", fill = "grey") + 
  #geom_sf(data = ec_localm, fill = NA, color = "red") +
  #ggtitle("Eastern cape") +
  #annotation_north_arrow(which_north = "grid",location = "topleft")+
  #scale_fill_manual(values = c("low" = "cornflowerblue",
                              #"medium" = "goldenrod2",
                               #"high" = "red")) +
  #coord_sf()
  #basic

##Setting out the plot areas many maps in one sheet
plot.new()
frame()
par(mfcol=c(1,1))

#Map Plot with Legends
gp <- ggplot() +
  geom_sf(data = ec_localm, aes(color = 'A'), fill = 'white', show.legend = 'polygon') +
  geom_sf(data = ec_district, aes(color = 'B'), lwd = 1, fill = 'transparent', show.legend = 'polygon') +
  scale_color_manual(values = c("A" = "green", "B" = "black"), 
                     labels = c("Local Municipalities", "District Municipalities"),
                     name = (NULL)) +
  ggtitle("The Eastern Cape") +
  annotation_north_arrow(which_north = "grid",location = "topleft")+
  theme(legend.position = c(0.9,0.1))+
  annotation_scale() 
gp




#Maiking a Map #->stationPrecipitation
mapview(ec_stations, xcol = "Longitude", ycol = "Latitude", crs = 4326, grid = FALSE, color="green")

###########################################################################
















