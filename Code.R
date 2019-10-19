
setwd("S:/Office 365/OneDrive for Business")
install.packages("leaflet")
require(leaflet)
require(dplyr)
install.packages("rgdal")
require(rgdal)
install.packages("sf")
library(sf)

codes <- st_read(dsn="postcodes", layer="geo_export_4c26d03b-b245-4b14-b132-edcc345bb6ee")
tree_canopy <- st_read(dsn="tree_canopy", layer="geo_export_a93a561d-def0-467a-8ae9-9b11b5139d6c")
# shp_tree2 <- st_read(dsn="tree_canopy", layer="geo_export_0b09438b-689e-4dbf-adc4-dc26ec3856d6")

int2 <- as_tibble(st_intersection(codes,tree_canopy))




map <- leaflet() %>%
  addTiles() %>% addPolygons(data = codes,color = "blue",
                             weight = 1,
                             smoothFactor = 0.5,
                             opacity = 1.0,
                             fillOpacity = 0.5,
                             highlightOptions = highlightOptions(color = "white",
                                                                 weight = 2,
                                                                 bringToFront = FALSE),label=~codes$mccid_int) %>% 
addPolygons(data =  tree_canopy$geometry,color = "red",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 1.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = FALSE)) %>%
  addPolygons(data =  int2$geometry,color = "yellow",
              weight = 1,
              smoothFactor = 0.5,
              opacity = 2.0,
              fillOpacity = 0.5,
              highlightOptions = highlightOptions(color = "white",
                                                  weight = 2,
                                                  bringToFront = FALSE))






int2$area = as.numeric(int2$area)
TreesbyCode <- aggregate(int2$area,by=list(int2$mccid_int),FUN=sum, na.rm=TRUE)

# renaming columns in a complicated way
TreesbyCode$TreeArea=TreesbyCode$x
TreesbyCode$mccid_int=TreesbyCode$Group.1
TreesbyCode$x=NULL
TreesbyCode$Group.1=NULL
TreesbyCode

final <- merge(codes,TreesbyCode,by="mccid_int",, all.x=TRUE)
final[is.na(final)] <- 0
final

pal_fun <- colorQuantile("YlOrRd", NULL, n = 10)


map <- leaflet() %>%
  addTiles() %>% addPolygons(data = final,fillColor = ~pal_fun(TreeArea),
                             stroke = TRUE,
                             weight = 2,
                             smoothFactor = 0.5,
                             fillOpacity = 0.8,
                             label=~final$mccid_int) %>%
  addLegend(data = final,"bottomright",  # location
             pal=pal_fun,    # palette function
             values=~final$TreeArea,  # value to be passed to palette function
             title = 'Melbourne tree canopy density per sqkm')
map

  