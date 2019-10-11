
########## move to script.R file later ##########

# install.packages("devtools")
library(devtools)
install_github("ropensci/drake")
library(drake)

library(sf)
library(ggplot2)
library(sp)
library(tictoc)
library(tigris)



kaggle_data_read <- drake_plan(
  # replace with correct directory. Can use file.choose()
  train = read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv"), 
  # replace with correct directory. Can use file.choose()
  test = read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 
)



########## specific merge_census code ##########

#### my personal append_geoid function, renamed as my_append_geoid

my_append_geoid <- function(train_city, state_boundary_file) {
  
  
  # Read in Block Group shapefile
  
  state_boundary <- st_read(state_boundary_file)
  
  # get the GEOID
  
  GEOID <- as.numeric(as.character(state_boundary$GEOID))
  
  # Convert shapefile into Polygons object
  
  state_coord <- as_Spatial(state_boundary$geometry)
  
  
  
  # Extract a mapping between geoid and its longitude/latitude polygon
  
  bg <- lapply(
    state_coord@polygons,
    function(x) {
      pre_coords <- x@Polygons[[1]]@coords
      coords <- data.frame(latitude = pre_coords[,2], longitude = pre_coords[,1])
      pre_id <- x@ID
      id <- as.integer((gsub("ID", "", pre_id))) # this is simply to get the order of block group (1st, 100th, etc.)
      list(id = GEOID[id], coords = coords)
    }
  )
  
  # This will assign each city intersection a geoid
  
  GeoID <- vector("numeric")
  
  
  
  for (i in 1:length(bg)){
    x = bg[[i]]$coords 
    indicator = point.in.polygon(train_city$Latitude, train_city$Longitude, x$latitude, x$longitude) == 1
    GeoID[indicator] <- bg[[i]]$id
  }
  
  
  
  # attach geoid to train_city
  
  train_city$GeoID <- GeoID
  
  return(train_city)
  
}



# This is a second resort: in the case that my method above cannot match the block group, I use the tigris package to do it. 
# The reason I don't use the tigris version of append_geoid only is because it would take way too long to use it for all the records.

my_append_geoid2 <- function(train_city) {
  
  if (train_city$City[1] != "Philadelphia") {
    
    missing_geoid <- which(is.na(train_city$GeoID))
    
    for (row_i in missing_geoid) {
      
      lat <- train_city[row_i,]$Latitude
      lon <- train_city[row_i,]$Longitude
      
      GeoID_append <- append_geoid(data.frame("lat" = lat, "lon" = lon), "block group")[3]
      
      train_city$GeoID[row_i] <- as.numeric(GeoID_append)
      
    } 
    
  } else if (train_city$City[1] == "Philadelphia") {
    
    # For some reason, the above code takes way too long for train_Philadelphia.
    # thus, I hard-code the missing GeoID. As it turns out, there's only one missing GeoID for Philadelphia
    train_city[is.na(train_city$GeoID),]$GeoID = 421010118004
    
  }
  
  return(train_city)
  
}



### Atlanta ###

train_Atlanta_append_GeoID <- drake_plan(
  # Extract Atlanta intersections
  train_Atlanta = train[train$City == "Atlanta",],
  # Read in Block Group shapefile
  GA_boundary_file = "tl_2019_13_bg/tl_2019_13_bg.shp",
  # append the GeoID to train_Atlanta
  train_Atlanta1 = my_append_geoid(train_Atlanta, GA_boundary_file),
  train_Atlanta2 = my_append_geoid2(train_Atlanta1)
)


### Boston ###

train_Boston_append_GeoID <- drake_plan(
  # Extract Boston intersections
  train_Boston = train[train$City == "Boston",],
  # Read in Block Group shapefile
  MA_boundary_file = "tl_2019_25_bg/tl_2019_25_bg.shp",
  # append the GeoID to train_Boston
  train_Boston1 = my_append_geoid(train_Boston, MA_boundary_file),
  train_Boston2 = my_append_geoid2(train_Boston1)
)



### Chicago ###

train_Chicago_append_GeoID <- drake_plan(
  # Extract Chicago intersections
  train_Chicago = train[train$City == "Chicago",],
  # Read in Block Group shapefile
  IL_boundary_file = "tl_2019_17_bg/tl_2019_17_bg.shp",
  # append the GeoID to train_Chicago
  train_Chicago1 = my_append_geoid(train_Chicago, IL_boundary_file),
  train_Chicago2 = my_append_geoid2(train_Chicago1)
)



### Philadelphia ###

train_Philadelphia_append_GeoID <- drake_plan(
  # Extract Philadelphia intersections
  train_Philadelphia = train[train$City == "Philadelphia",],
  # Read in Block Group shapefile
  PA_boundary_file = "tl_2019_42_bg/tl_2019_42_bg.shp",
  # append the GeoID to train_Philadelphia
  train_Philadelphia1 = my_append_geoid(train_Philadelphia, PA_boundary_file),
  train_Philadelphia2 = my_append_geoid2(train_Philadelphia1)
)



#### Merging Land Area of each block group ####

merge_land_area <- function(train_city, state_boundary_file) {
  
  state_boundary <- st_read(state_boundary_file)
  
  GeoID <- as.numeric(as.character(state_boundary$GEOID))
  
  LandArea <- state_boundary$ALAND
  
  train_city <- merge(train_city, cbind(GeoID, LandArea), by = "GeoID", all.x = TRUE)
  
  return(train_city)
  
}



train_city_append_LandArea <- drake_plan(
  # append the LandArea to train_Atlanta
  train_Atlanta3 = merge_land_area(train_Atlanta2, GA_boundary_file),
  train_Boston3 = merge_land_area(train_Boston2, MA_boundary_file),
  train_Chicago3 = merge_land_area(train_Chicago2, IL_boundary_file),
  train_Philadelphia3 = merge_land_area(train_Philadelphia2, PA_boundary_file)
)




#### Merging in Total Population Data ####

merge_acs <- function(train_city, acs_file_name, old_var_name, new_var_name) {
  
  acs_file <- read.csv(acs_file_name, header = TRUE, skip = 1)
  
  names(acs_file)[names(acs_file) == "Id2"] <- "GeoID"
  
  train_city <- merge(train_city, acs_file[c("GeoID", old_var_name)], by = "GeoID", all.x = TRUE)
  
  names(train_city)[names(train_city) == old_var_name] <- new_var_name
  
  return(train_city)
  
}



### Atlanta ###

train_Atlanta_append_TotalPopulation <- drake_plan(
  GA_acs_file_name = "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/GA_total_popl.csv",
  train_Atlanta4 = merge_acs(train_Atlanta3, GA_acs_file_name, "Estimate..Total", "TotalPopulation")
)



### Boston ###

train_Boston_append_TotalPopulation <- drake_plan(
  MA_acs_file_name = "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/MA_total_popl.csv",
  train_Boston4 = merge_acs(train_Boston3, MA_acs_file_name, "Estimate..Total", "TotalPopulation")
)



### Chicago ###

train_Chicago_append_TotalPopulation <- drake_plan(
  IL_acs_file_name = "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/IL_Cook_County_total_popl.csv",
  train_Chicago4 = merge_acs(train_Chicago3, IL_acs_file_name, "Estimate..Total", "TotalPopulation")
)



### Philadelphia ###

train_Philadelphia_append_TotalPopulation <- drake_plan(
  PA_acs_file_name = "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/PA_total_popl.csv",
  train_Philadelphia4 = merge_acs(train_Philadelphia3, PA_acs_file_name, "Estimate..Total", "TotalPopulation")
)



merge_census <- rbind(
  kaggle_data_read,
  train_Atlanta_append_GeoID,
  train_Boston_append_GeoID,
  train_Chicago_append_GeoID,
  train_Philadelphia_append_GeoID,
  train_city_append_LandArea,
  train_Atlanta_append_TotalPopulation,
  train_Boston_append_TotalPopulation,
  train_Chicago_append_TotalPopulation,
  train_Philadelphia_append_TotalPopulation,
  drake_plan(
    train_append_TotalPopulation = rbind.data.frame(train_Atlanta4, train_Boston4, train_Chicago4, train_Philadelphia4)
  )
)



config <- drake_config(merge_census)

vis_drake_graph(config)




make(merge_census)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()







