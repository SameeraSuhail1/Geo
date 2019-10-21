

setwd("S:/Office 365/OneDrive for Business/maps")
install.packages("leaflet")
require(leaflet)
require(dplyr)
install.packages("rgdal")
require(rgdal)
install.packages("sf")
library(sf)

codes <- st_read(dsn="postcodes", layer="geo_export_4c26d03b-b245-4b14-b132-edcc345bb6ee")
tree_canopy <- st_read(dsn="tree_canopy", layer="geo_export_be458962-4d98-403b-98f6-567c0ed6703c")
tree_canopy2 <- st_read(dsn="tree_canopy", layer="geo_export_5b49efe9-27af-426d-8283-ba77e2c7f1ae")
street_lights <- st_read(dsn="street_lights", layer="geo_export_430a08c9-7ad8-4ae1-9797-88061f0571f3")

#tree_canopy_f <- rbind(tree_canopy,tree_canopy2)

source("utility.R")
tree_density<-find_tree_density(codes,tree_canopy)
plot_density(tree_density,"TreeArea","tree canopy")

lux_density <- find_lux_density(codes, street_lights)
plot_density(lux_density,"TotalLux","light intensity")


tree_density_grid <- find_tree_density_grid(codes,tree_canopy)
lux_density_grid <- find_lux_density_grid(codes,street_lights)


plot_density_grid(tree_density_grid,"TreeArea")
plot_density_grid(lux_density_grid,"TotalLux")


