install.packages("randomForest")
library(randomForest)



set.seed(100)
train <- sample(nrow(house_table), 0.7*nrow(house_table), replace = FALSE)
TrainSet <- house_table[train,]
ValidSet <- house_table[-train,]
summary(TrainSet)
summary(ValidSet)



model1 <- randomForest(Price~tree_distance + light_distance + playg_distance + featurelight_distance +  Rooms + height
                       + metro_distance +syringebin_distance + cafe_distance+ bar_distance,
                       data = TrainSet, importance = TRUE)
model1


house_table$predicted_priceRF <- predict(model1, house_table)
house_table$residualRF <- house_table$Price - house_table$predicted_priceRF





#--------------------------------Linear Regression--------------------------
see_fit1 = lm(Price~tree_distance + light_distance + playg_distance + featurelight_distance +  Rooms + height
              + metro_distance +syringebin_distance + cafe_distance+ bar_distance, data=house_table)
summary(see_fit1)

house_table$predicted_price <- predict(see_fit1)
house_table$residuals <- residuals(see_fit1) 

#-------------------------------compare rmse of lm and RF-------------------
rmse(house_table$Price, house_table$predicted_priceRF)
rmse(house_table$Price, house_table$predicted_price)


#------------plot Violin plots-------------------------


temp1 <- as.data.frame(house_table$residuals)
temp1$model <- 1
temp1["residuals"] <- temp1["house_table$residuals"]
temp1["house_table$residuals"] <-  NULL


temp2 <- as.data.frame(house_table$residualRF)
temp2$model <- 2
temp2["residuals"] <- temp2["house_table$residualRF"]
temp2["house_table$residualRF"] <-  NULL


temp <- rbind(temp1,temp2)

jpeg("violin_RF_LM.jpeg")
ggplot(temp, aes(x=as.factor(model), y=residuals)) + 
  geom_violin() + labs(x="Model", y="Residuals") +
  scale_x_discrete(labels=c("1" = "Linear Model", "2" = "Random Forest"))
dev.off()


#-------------true vs predicted Price (Random Forest)--------------------------------

jpeg('true_vs_pred_plot.jpg')
ggplot(house_table)+
  geom_point(aes(x = Price, y = predicted_priceRF))+
  geom_abline(intercept = 0, slope = 1)
dev.off()


#--------------------------scatter residual plot (Random Forest)---------------------------
temp <- house_table
temp$bar_distance <- as.numeric(temp$bar_distance)
temp$metro_distance <- as.numeric(temp$metro_distance)

jpeg("metro_residual.jpeg")
ggplot(temp, aes_string(x="metro_distance", y="residualRF")) + 
  geom_point()  + labs(x="Property's distance from Metro Station (m)", y="Residual (m)")
dev.off()

jpeg("bar_residual.jpeg")
ggplot(temp, aes_string("bar_distance", y="residualRF")) + 
  geom_point()  + labs(x="Property's distance from nearest bar (m)", y="Residual (m)")
dev.off()




#--------------------------plot metro distance vs. true price-----------------------------------------------------



temp <- house_table
temp$metro_distance <- as.numeric(temp$metro_distance)

jpeg("Metro_TruePrice.jpeg")
ggplot(temp, aes_string(x="metro_distance", y="Price")) + 
  geom_point()  + labs(x="Distance from Metro", y="True Price")
dev.off()

#-----------------------plot metro vs residual using only room predictor (RF)------------------------------

house_table2 <- house_table
model2 <- randomForest(Price~ Rooms , data = TrainSet, importance = TRUE)
model2


#see_fit2 = lm(Price~Rooms, data=house_table2)
#summary(see_fit2)


house_table2$predicted_price <- predict(model2, house_table2)
house_table2$residuals <- house_table2$Price - house_table2$predicted_price
house_table2$metro_distance <- as.numeric(house_table2$metro_distance)

jpeg("Metro_Residual_byRoom.jpeg")
ggplot(house_table2, aes_string(x="metro_distance", y="residuals")) + 
  geom_point()  + labs(x="Distance from Metro", y="Residuals")
dev.off()





#sort in order of increasing absolute residuals


house_table2 <- house_table
house_table2 <- as.data.frame(select(house_table2, -c(geometry,geometry.1,geometry.2,geometry.3,geometry.4,geometry.5,geometry.6,geometry.7,geometry.8)))
residual_sorted_tableRF <- merge(as.data.frame(origAddress_final), house_table2, by="rownumber_init", all=FALSE)


residual_sorted_tableRF2 <- residual_sorted_tableRF[order(residual_sorted_tableRF$residualRF),]

residual_sorted_tableRF2$relative_errorRF <- abs(residual_sorted_tableRF2$residualRF)/residual_sorted_tableRF2$Price


residual_sorted_tableRF2 <- as.data.frame(select(residual_sorted_tableRF2, -c(geometry.y)))
residual_sorted_tableRF2 <- st_as_sf(residual_sorted_tableRF2)
st_crs(residual_sorted_tableRF2) = 4326
residual_sorted_tableRF2 <- st_transform(residual_sorted_tableRF2,"+proj=longlat +ellps=WGS84")





pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)
jpeg('residual_intensity.jpeg')
leaflet() %>%
  addTiles() %>% addPolygons(data = codes,stroke = TRUE, color = "black",
                             weight = 2, opacity = 1, fillColor = "blue",
                             smoothFactor = 0.5,
                             fillOpacity = 0.5,label=~codes$mccid_int,
                             highlightOptions = highlightOptions(color = "red", stroke = TRUE, weight= 2, opacity =1,
                                                                 bringToFront = FALSE, sendToBack = TRUE)) %>% 
  addCircleMarkers(data = residual_sorted_tableRF2, radius = 3, color = ~pal_fun(residual_sorted_tableRF2$residualRF)) %>%
  addLegend(data = residual_sorted_tableRF2, "topright", pal=pal_fun, values = residual_sorted_tableRF2$residualRF,
            title = "Price residual for each house")
dev.off()