
find_tree_density <- function(codes,file2) {
  
  int2 <- as_tibble(st_intersection(codes,file2))
  
  int2$area = as.numeric(int2$area)
  TreesbyCode <- aggregate(int2$area,by=list(int2$mccid_int),FUN=sum, na.rm=TRUE)
  
  # renaming columns in a complicated way
  TreesbyCode$TreeArea=TreesbyCode$x
  TreesbyCode$mccid_int=TreesbyCode$Group.1
  TreesbyCode$x=NULL
  TreesbyCode$Group.1=NULL
  TreesbyCode
  
  final_file <- merge(codes,TreesbyCode,by="mccid_int",, all.x=TRUE)
  final_file[is.na(final_file)] <- 0
  return(final_file)
}

#------------------------------------------------------------------------------------------------------


find_lux_density <- function(codes,street_lights) {
  int2 <- as_tibble(st_intersection(codes,street_lights))
  
  int2$label = as.numeric(int2$label)
  LuxbyCode <- aggregate(int2$label,by=list(int2$mccid_int),FUN=sum, na.rm=TRUE)
  
  # renaming columns in a complicated way
  LuxbyCode$TotalLux=LuxbyCode$x
  LuxbyCode$mccid_int=LuxbyCode$Group.1
  LuxbyCode$x=NULL
  LuxbyCode$Group.1=NULL
  LuxbyCode
  
  
  final_file <- merge(codes,LuxbyCode,by="mccid_int",, all.x=TRUE)
  final_file$TotalLux[is.na(final_file$TotalLux)] <- 0
  return(final_file) 
}


#------------------------------------------------------------------------------------------------
find_playground_number <- function(codes,file2) {
  
  codes$code_area <- st_area(codes)
  int2 <- as_tibble(st_intersection(codes,file2))
  int2$num <- 1
  PGbyCode <- aggregate(list(num=int2$num,code_area=int2$code_area), by=list(int2$mccid_int),FUN=sum, na.rm =TRUE)
  
  PGbyCode$numpersqm = PGbyCode$num/PGbyCode$code_area
  colnames(PGbyCode)[colnames(PGbyCode)=="Group.1"] <- "mccid_int"
  
  final_file <- merge(codes,PGbyCode,by="mccid_int",, all.x=TRUE)
  final_file[is.na(final_file)] <- 0
  final_file$code_area.y =NULL
  colnames(final_file)[colnames(final_file)=="code_area.x"] <- "code_area"
  return(final_file)
}



#----------------------------------------------------------------------------------------------











plot_density <- function(final_file,col_name,s) {
  pal_fun <- colorQuantile("YlOrRd", NULL, n = 3)
  
  
  map <- leaflet() %>%
    addTiles() %>% addPolygons(data = final_file,fillColor = ~pal_fun(final_file[[col_name]]),
                               stroke = TRUE,
                               weight = 2,
                               smoothFactor = 0.5,
                               fillOpacity = 0.8,
                               label=~final_file$mccid_int) %>%
    addLegend(data = final_file,"bottomright",  # location
              pal=pal_fun,    # palette function
              values=~final_file[[col_name]],  # value to be passed to palette function
              title = paste("Melbourne", s, "density per sqm"))
  return(map)
}

make_grid <- function(codes,n){
  mel_city<- st_union(codes)
  #grid only inside the boundary of Melbourne city
  polygon_grid <- st_make_grid(mel_city, square = T, n) %>% st_intersection(mel_city)
  return(polygon_grid)
}


plot_grid <- function(polygon_grid){
  mel_grid <- leaflet() %>%
    addTiles() %>%  addPolygons(data = polygony,color = "red",
                                weight = 0.5,
                                fillColor = "blue",
                                smoothFactor = 0.5,
                                opacity = 1.0,
                                fillOpacity = 0.5,
                                highlightOptions = highlightOptions(color = "white",
                                                                    weight = 2,
                                                                    bringToFront = FALSE))
  return(mel_grid)
}





find_tree_density_grid <- function(codes, file2) {
  mel_city<- st_union(codes)
  g <- mel_city %>%
    st_make_grid(n=100) %>%
    st_intersection(mel_city) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(id = row_number())
  
  
  int2 <- as_tibble(st_intersection(g,file2))
  
  
  
  int2$area = as.numeric(int2$area)
  TreesbyID <- aggregate(int2$area,by=list(int2$id),FUN=sum, na.rm=TRUE)
  
  TreesbyID$TreeArea=TreesbyID$x
  TreesbyID$id=TreesbyID$Group.1
  TreesbyID$x=NULL
  TreesbyID$Group.1=NULL
  TreesbyID
  
  
  final_file <- merge(g,TreesbyID,by="id",, all.x=TRUE)
  final_file[is.na(final_file)] <- 0
  return(final_file)
}


find_lux_density_grid <- function(codes, file2) {
  mel_city<- st_union(codes)
  g <- mel_city %>%
    st_make_grid(n=100) %>%
    st_intersection(mel_city) %>%
    st_cast("MULTIPOLYGON") %>%
    st_sf() %>%
    mutate(id = row_number())
  
  
  int2 <- as_tibble(st_intersection(g,file2))
  
  
  
  int2$label = as.numeric(int2$label)
  LUXbyID <- aggregate(int2$label,by=list(int2$id),FUN=sum, na.rm=TRUE)
  
  LUXbyID$TotalLux=LUXbyID$x
  LUXbyID$id=LUXbyID$Group.1
  LUXbyID$x=NULL
  LUXbyID$Group.1=NULL
  LUXbyID
  
  
  final_file <- merge(g,LUXbyID,by="id",, all.x=TRUE)
  final_file[is.na(final_file)] <- 0
  return(final_file)
}

plot_density_grid <- function(final_file, col_name) {
  
  
  bins <- c(0,10, 100, 1000, 10000, 100000, 1000000, Inf)
  pal_fun <- colorBin("Greens", domain = final_file$TreeArea, bins = bins)
  
  map <- leaflet() %>%
    addTiles() %>% addPolygons(data = final_file,fillColor = ~pal_fun(final_file[[col_name]]),
                               stroke = TRUE,
                               weight = 1,
                               smoothFactor = 0.5,
                               fillOpacity = 2,
                               label=~final_file$id) %>%
    addLegend(data = final_file,"bottomright",  # location
              pal=pal_fun,    # palette function
              values=~final_file[[col_name]],  # value to be passed to palette function
              title = 'Melbourne tree canopy density per sqm')
  return(map)
  
}


scatter_plot_it <- function(data1,data2,col_name1,col_name2,xlabel=col_name1,ylabel=col_name2){
  
  data1_2 <- merge(as.data.frame(data1), as.data.frame(data2), by="mccid_int", all.x=TRUE)
  data1_2 <- st_as_sf(data1_2)
  ggplot(data1_2, aes_string(x=col_name1, y=col_name2)) + 
    geom_point() +geom_text_repel(aes(label=mccid_int)) + labs(x=xlabel, y=ylabel)
  
}





find_nearest_distance <- function(origAddress, codes,file,s) {
  
  origAddress<-readRDS("origAddress_final.rds")
  # Make it a sf object
  origAddress_sf <- st_as_sf(origAddress)
  st_crs(origAddress_sf) = 4326
  origAddress_sf<-st_transform(origAddress_sf, "+proj=longlat +ellps=WGS84")
  
  
  #find only those houses which are in our postcodes
  house_in_codes<- st_intersection(codes,origAddress_sf)
  house_in_codes<-house_in_codes[!is.na(house_in_codes$Price),]
  
  
  tree_code<-st_intersection(codes,file)
  
  
  #for each feature (geometry) in house_in_codes, it returns the index of the nearest feature (geometry) in tree_code
  temp<- as.vector(st_nearest_feature(house_in_codes, tree_code))
  
  tree_code$rownumber = 1:nrow(tree_code)
  
  nearest_tree <- tree_code[temp,]
  
  house_tree_distance<-as.data.frame(st_distance(house_in_codes,nearest_tree, by_element = TRUE))
  colnames(house_tree_distance) <- s
  
  house_table <- cbind(as.data.frame(house_in_codes),house_tree_distance)
  house_table <- st_as_sf(house_table)
  st_crs(house_table) = 4326
  house_table<-st_transform(house_table, "+proj=longlat +ellps=WGS84")
  
  return(house_table)
  
}