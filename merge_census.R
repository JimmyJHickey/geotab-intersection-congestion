
library(sf)
library(ggplot2)
library(sp)
library(tictoc)

# replace with correct directory. Can use file.choose()
train <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv") 

# replace with correct directory. Can use file.choose()
test <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 



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
  
  geoid <- vector("integer")
  
  
  
  for (i in 1:length(bg)){
    x = bg[[i]]$coords 
    indicator = point.in.polygon(train_city$Latitude, train_city$Longitude, x$latitude, x$longitude) == 1
    geoid[indicator] <- bg[[i]]$id
  }
  
  
  
  # attach geoid to train_city
  
  train_city$geoid <- geoid
  
  return(train_city)
  
}







#### Chicago ####

# Extract Chicago intersections

train_Chicago <- train[train$City == "Chicago",] 

# Read in Block Group shapefile

IL_boundary_file <- "tl_2019_17_bg/tl_2019_17_bg.shp"







# append the geoid to train_Chicago

tic()

train_Chicago <- my_append_geoid(train_Chicago, IL_boundary_file)

toc()












# # get the GEOID
# 
# geoid <- as.numeric(as.character(IL_boundary$GEOID))
# 
# # Convert shapefile into Polygons object
# 
# IL_coord <- as_Spatial(IL_boundary$geometry)
# 
# 
# 
# # Extract a mapping between OBJECTID (block group number) and its longitude/latitude polygon
# 
# bg <- lapply(
#   IL_coord@polygons,
#   function(x) {
#     pre_coords <- x@Polygons[[1]]@coords
#     coords <- data.frame(latitude = pre_coords[,2], longitude = pre_coords[,1])
#     pre_id <- x@ID
#     id <- as.integer((gsub("ID", "", pre_id))) # this is simply to get the order of block group (1st, 100th, etc.)
#     list(id = geoid[id], coords = coords)
#   }
# )
# 
# 
# 
# # This will assign each Chicago intersection a geoid
# 
# geoid <- vector("integer")
# 
# 
# 
# # Takes about 17 minutes
# 
# tic()
# 
# for (i in 1:length(bg)){
#   x = bg[[i]]$coords 
#   indicator = point.in.polygon(train_Chicago$Latitude, train_Chicago$Longitude, x$latitude, x$longitude) == 1
#   geoid[indicator] <- bg[[i]]$id
# }
# 
# toc()
# 
# 
# 
# # attach geoid to train_Chicago
# 
# train_Chicago$geoid <- geoid










# read in population data

chicago_popl <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/ACS_17_5YR_B01003_with_ann.csv", 
                         header = TRUE, skip = 1)

names(chicago_popl)[names(chicago_popl) == "Id2"] <- "geoid"



train_Chicago <- merge(train_Chicago, chicago_popl[c("geoid", "Estimate..Total")], by = "geoid", all.x = TRUE)

save(train_Chicago, file = "backup_data_files/train_Chicago.RData")

















