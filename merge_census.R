
library(sf)
library(ggplot2)
library(sp)
library(tictoc)

# replace with correct directory. Can use file.choose()
train <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv") 

# replace with correct directory. Can use file.choose()
test <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 



# Extract Chicago intersections

train_Chicago <- train[train$City == "Chicago",]



# Read in Block Group shapefile

IL_boundary <- st_read("tl_2019_17_bg/tl_2019_17_bg.shp")

# get the GEOID

geoid <- as.numeric(as.character(IL_boundary$GEOID))

# Convert shapefile into Polygons object

IL_coord <- as_Spatial(IL_boundary$geometry)



# Extract a mapping between OBJECTID (block group number) and its longitude/latitude polygon

blocks <- lapply(
  IL_coord@polygons,
  function(x) {
    pre_coords <- x@Polygons[[1]]@coords
    coords <- data.frame(latitude = pre_coords[,2], longitude = pre_coords[,1])
    pre_id <- x@ID
    id <- as.integer((gsub("ID", "", pre_id)))
    list(id = geoid[id], coords = coords)
  }
)



# This will assign each Chicago intersection a geoid

block_id <- vector("integer")



# Takes about 17 minutes

tic()

for (i in 1:length(blocks)){
  x = blocks[[i]]$coords 
  indicator = point.in.polygon(train_Chicago$Latitude, train_Chicago$Longitude, x$latitude, x$longitude) == 1
  block_id[indicator] <- blocks[[i]]$id
}

toc()















