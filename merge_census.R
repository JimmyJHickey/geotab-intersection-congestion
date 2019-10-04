
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



#### Atlanta ####

# Extract Atlanta intersections

train_Atlanta <- train[train$City == "Atlanta",] 

# Read in Block Group shapefile

GA_boundary_file <- "tl_2019_13_bg/tl_2019_13_bg.shp"

# append the geoid to train_Atlanta

tic()

train_Atlanta <- my_append_geoid(train_Atlanta, GA_boundary_file)

toc()



#### Boston ####

# Extract Boston intersections

train_Boston <- train[train$City == "Boston",] 

# Read in Block Group shapefile

MA_boundary_file <- "tl_2019_25_bg/tl_2019_25_bg.shp"

# append the geoid to train_Boston

tic()

train_Boston <- my_append_geoid(train_Boston, MA_boundary_file)

toc()



#### Philadelphia ####

# Extract Philadelphia intersections

train_Philadelphia <- train[train$City == "Philadelphia",] 

# Read in Block Group shapefile

PA_boundary_file <- "tl_2019_42_bg/tl_2019_42_bg.shp"

# append the geoid to train_Philadelphia

tic()

train_Philadelphia <- my_append_geoid(train_Philadelphia, PA_boundary_file)

toc()








#### merging in population data ####

chicago_popl <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/ACS_17_5YR_B01003_with_ann.csv", 
                         header = TRUE, skip = 1)

names(chicago_popl)[names(chicago_popl) == "Id2"] <- "geoid"



train_Chicago <- merge(train_Chicago, chicago_popl[c("geoid", "Estimate..Total")], by = "geoid", all.x = TRUE)

save(train_Chicago, file = "backup_data_files/train_Chicago.RData")






















