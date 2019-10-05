
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
  
  GeoID <- vector("integer")
  
  
  
  for (i in 1:length(bg)){
    x = bg[[i]]$coords 
    indicator = point.in.polygon(train_city$Latitude, train_city$Longitude, x$latitude, x$longitude) == 1
    GeoID[indicator] <- bg[[i]]$id
  }
  
  
  
  # attach geoid to train_city
  
  train_city$GeoID <- GeoID
  
  return(train_city)
  
}







#### Chicago ####

# Extract Chicago intersections

train_Chicago <- train[train$City == "Chicago",] 

# Read in Block Group shapefile

IL_boundary_file <- "tl_2019_17_bg/tl_2019_17_bg.shp"

# append the GeoID to train_Chicago

tic()

train_Chicago <- my_append_geoid(train_Chicago, IL_boundary_file)

toc()



#### Atlanta ####

# Extract Atlanta intersections

train_Atlanta <- train[train$City == "Atlanta",] 

# Read in Block Group shapefile

GA_boundary_file <- "tl_2019_13_bg/tl_2019_13_bg.shp"

# append the GeoID to train_Atlanta

tic()

train_Atlanta <- my_append_geoid(train_Atlanta, GA_boundary_file)

toc()



#### Boston ####

# Extract Boston intersections

train_Boston <- train[train$City == "Boston",] 

# Read in Block Group shapefile

MA_boundary_file <- "tl_2019_25_bg/tl_2019_25_bg.shp"

# append the GeoID to train_Boston

tic()

train_Boston <- my_append_geoid(train_Boston, MA_boundary_file)

toc()



#### Philadelphia ####

# Extract Philadelphia intersections

train_Philadelphia <- train[train$City == "Philadelphia",] 

# Read in Block Group shapefile

PA_boundary_file <- "tl_2019_42_bg/tl_2019_42_bg.shp"

# append the GeoID to train_Philadelphia

tic()

train_Philadelphia <- my_append_geoid(train_Philadelphia, PA_boundary_file)

toc()







#### Merging in Total Population Data ####

merge_acs <- function(train_city, acs_file_name, old_var_name, new_var_name) {
  
  acs_file <- read.csv(acs_file_name, header = TRUE, skip = 1)
  
  names(acs_file)[names(acs_file) == "Id2"] <- "GeoID"
  
  train_city <- merge(train_city, acs_file[c("GeoID", old_var_name)], by = "GeoID", all.x = TRUE)
  
  names(train_city)[names(train_city) == old_var_name] <- new_var_name
  
  return(train_city)
  
}

### Chicago ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/IL_Cook_County_total_popl.csv"

train_Chicago <- merge_acs(train_Chicago, acs_file_name, "Estimate..Total", "TotalPopulation")

save(train_Chicago, file = "backup_data_files/train_Chicago.RData")



### Atlanta ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/GA_total_popl.csv"

train_Atlanta <- merge_acs(train_Atlanta, acs_file_name, "Estimate..Total", "TotalPopulation")

save(train_Atlanta, file = "backup_data_files/train_Atlanta.RData")



### Boston ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/MA_total_popl.csv"

train_Boston <- merge_acs(train_Boston, acs_file_name, "Estimate..Total", "TotalPopulation")

save(train_Boston, file = "backup_data_files/train_Boston.RData")



### Philadelphia ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/PA_total_popl.csv"

train_Philadelphia <- merge_acs(train_Philadelphia, acs_file_name, "Estimate..Total", "TotalPopulation")

save(train_Philadelphia, file = "backup_data_files/train_Philadelphia.RData")




train <- rbind.data.frame(train_Atlanta, train_Boston, train_Chicago, train_Philadelphia)

save(train, file = "new_train.RData")















