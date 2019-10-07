
library(sf)
library(ggplot2)
library(sp)
library(tictoc)
library(tigris)

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



# This is a second resort: in the case that my method above cannot match the block group, I use the tigris package to do it. 
# The reason I don't use the tigris version of append_geoid is because it would take way too long to use it for all the records.

my_append_geoid2 <- function(train_city) {
  
  missing_geoid <- which(is.na(train_city$GeoID))
  
  for (row_i in missing_geoid) {
    
    lat <- train_city[row_i,]$Latitude
    lon <- train_city[row_i,]$Longitude
    
    GeoID_append <- append_geoid(data.frame("lat" = lat, "lon" = lon), "block group")[3]
    
    train_city$GeoID[row_i] <- GeoID_append
    
  }
  
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

train_Chicago <- my_append_geoid2(train_Chicago)

toc()



#### Atlanta ####

# Extract Atlanta intersections

train_Atlanta <- train[train$City == "Atlanta",] 

# Read in Block Group shapefile

GA_boundary_file <- "tl_2019_13_bg/tl_2019_13_bg.shp"

# append the GeoID to train_Atlanta

tic()

train_Atlanta <- my_append_geoid(train_Atlanta, GA_boundary_file)

train_Atlanta <- my_append_geoid2(train_Atlanta)

toc()



#### Boston ####

# Extract Boston intersections

train_Boston <- train[train$City == "Boston",] 

# Read in Block Group shapefile

MA_boundary_file <- "tl_2019_25_bg/tl_2019_25_bg.shp"

# append the GeoID to train_Boston

tic()

train_Boston <- my_append_geoid(train_Boston, MA_boundary_file)

train_Boston <- my_append_geoid2(train_Boston)

toc()



#### Philadelphia ####

# Extract Philadelphia intersections

train_Philadelphia <- train[train$City == "Philadelphia",] 

# Read in Block Group shapefile

PA_boundary_file <- "tl_2019_42_bg/tl_2019_42_bg.shp"

# append the GeoID to train_Philadelphia

tic()

train_Philadelphia <- my_append_geoid(train_Philadelphia, PA_boundary_file)

# For some reason, the below code takes way too long.

# train_Philadelphia <- my_append_geoid2(train_Philadelphia)

# thus, I hard-code the missing GeoID. As it turns out, there's only one missing GeoID

train_Philadelphia[is.na(train_Philadelphia$GeoID),]$GeoID <- 421010118004

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

saveRDS(train_Chicago, file = "backup_data_files/train_Chicago_append_TotalPopulation.rds")



### Atlanta ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/GA_total_popl.csv"

train_Atlanta <- merge_acs(train_Atlanta, acs_file_name, "Estimate..Total", "TotalPopulation")

saveRDS(train_Atlanta, file = "backup_data_files/train_Atlanta_append_TotalPopulation.rds")



### Boston ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/MA_total_popl.csv"

train_Boston <- merge_acs(train_Boston, acs_file_name, "Estimate..Total", "TotalPopulation")

saveRDS(train_Boston, file = "backup_data_files/train_Boston_append_TotalPopulation.rds")



### Philadelphia ###

acs_file_name <- "/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/PA_total_popl.csv"

train_Philadelphia <- merge_acs(train_Philadelphia, acs_file_name, "Estimate..Total", "TotalPopulation")

saveRDS(train_Philadelphia, file = "backup_data_files/train_Philadelphia_append_TotalPopulation.rds")




train <- rbind.data.frame(train_Atlanta, train_Boston, train_Chicago, train_Philadelphia)

saveRDS(train, file = "backup_data_files/train_append_TotalPopulation.rds")















