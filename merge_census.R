
library(sf)
library(ggplot2)
library(sp)
library(tictoc)

# replace with correct directory. Can use file.choose()
train <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv") 

# replace with correct directory. Can use file.choose()
test <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 



#### Chicago ####

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
    id <- as.integer((gsub("ID", "", pre_id))) # this is simply to get the order of block group (1st, 100th, etc.)
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



# attach block_id to train_Chicago

train_Chicago$block_id <- block_id



# read in population data

chicago_popl <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/ACS_17_5YR_B01003_with_ann.csv", 
                         header = TRUE, skip = 1)

names(chicago_popl)[names(chicago_popl) == "Id2"] <- "block_id"



train_Chicago <- merge(train_Chicago, chicago_popl[c("block_id", "Estimate..Total")], by = "block_id", all.x = TRUE)

save(train_Chicago, file = "backup_data_files/train_Chicago.RData")

















