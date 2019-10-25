

setwd("S:/Office 365/OneDrive for Business/maps")
install.packages("leaflet")
require(leaflet)
require(dplyr)
install.packages("rgdal")
require(rgdal)
install.packages("sf")
library(sf)
install.packages("lwgeom")
library(lwgeom)
install.packages("ggrepel")
library(ggrepel)
library(ggplot2)

install.packages("ggmap")
library(ggmap)
install.packages("tmaptools")
library(tmaptools)
install.packages("DataCombine")
library(DataCombine)


codes <- st_read(dsn="postcodes", layer="geo_export_4c26d03b-b245-4b14-b132-edcc345bb6ee")
tree_canopy <- st_read(dsn="tree_canopy", layer="geo_export_be458962-4d98-403b-98f6-567c0ed6703c")
tree_canopy2 <- st_read(dsn="tree_canopy", layer="geo_export_5b49efe9-27af-426d-8283-ba77e2c7f1ae")
street_lights <- st_read(dsn="street_lights", layer="geo_export_430a08c9-7ad8-4ae1-9797-88061f0571f3")
playgrounds <- st_read(dsn="playgrounds", layer="geo_export_3d9b48c4-f550-40df-93b8-b173f9dbde26")


#tree_canopy_f <- rbind(tree_canopy,tree_canopy2)

source("utility.R")
tree_density<-find_tree_density(codes,tree_canopy)
plot_density(tree_density,"TreeArea","tree canopy")

lux_density <- find_lux_density(codes, street_lights)
plot_density(lux_density,"TotalLux","light intensity")


playground_number<-find_playground_number(codes,playgrounds)
plot_density(playground_number,"num","playground")


# ------------------------See scatter plots between different variables---------------------
jpeg("scatter1.jpeg")
scatter_plot_it(tree_density,playground_number,col_name1 = "num",col_name2 = "TreeArea",
                xlabel = "No. of playgrounds", ylabel = "Tree Canopy Area")
dev.off()

jpeg("scatter2.jpeg")
scatter_plot_it(lux_density,playground_number, col_name1 = "num", col_name2 = "TotalLux",
                xlabel = "No. of playgrounds", ylabel = "Total Light Illuminance (Lux)")
dev.off()

jpeg("scatter3.jpeg")
scatter_plot_it(tree_density,lux_density, col_name1 = "TreeArea",col_name2 = "TotalLux",
                xlabel = "Tree Canopy Area", ylabel = "Total Light Illuminance (Lux)")
dev.off()
#----------------------Make Table------------------------------------------------------


origAddress_final<-readRDS("origAddress_final.rds")
table<- find_nearest_distance(origAddress_final, codes,tree_canopy, "tree_distance")
table2<- find_nearest_distance(origAddress_final, codes,street_lights, "light_distance") 
table3<- find_nearest_distance(origAddress_final, codes,playgrounds, "playg_distance") 
house_table <- cbind(table, table2["light_distance"],table3["playg_distance"])
house_table <- house_table[-c(749),]

#--------------------Linear Regression--------------------------
see_fit1 = lm(Price~tree_distance + light_distance + playg_distance + Rooms, data=house_table)
summary(see_fit1)


house_table$predicted_price <- predict(see_fit1)
house_table$residuals <- residuals(see_fit1)  
residual_sorted_table <- house_table[order(abs(house_table$residuals)),]

residual_sorted_table$relative_error <- abs(residual_sorted_table$residual)/residual_sorted_table$Price

jpeg('true_vs_pred_plot.jpg')
ggplot(residual_sorted_table, aes(x = Price, y = predicted_price))+
  geom_point(aes(color = abs(residuals), size = abs(residuals)))+
  geom_abline(intercept = 0, slope = 1)
dev.off()


pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
jpeg('residual_intensity.jpeg')
leaflet() %>%
  addTiles() %>% addPolygons(data = codes,stroke = TRUE, color = "black",
                             weight = 2, opacity = 1, fillColor = "blue",
                             smoothFactor = 0.5,
                             fillOpacity = 0.5,label=~codes$mccid_int,
                             highlightOptions = highlightOptions(color = "red", stroke = TRUE, weight= 2, opacity =1,
                                                                 bringToFront = FALSE, sendToBack = TRUE)) %>% 
  addCircleMarkers(data = residual_sorted_table, radius = 3, color = ~pal_fun(residual_sorted_table$residuals)) %>%
  addLegend(data = residual_sorted_table, "topright", pal=pal_fun, values = residual_sorted_table$residuals,
            title = "Price residual for each house")
dev.off()


#-------------------------------grid---------------------------------------

tree_density_grid <- find_tree_density_grid(codes,tree_canopy)
lux_density_grid <- find_lux_density_grid(codes,street_lights)





plot_density_grid(tree_density_grid,"TreeArea")
plot_density_grid(lux_density_grid,"TotalLux")


