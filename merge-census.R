
library(tigris)
library(tictoc)

# replace with correct directory. Can use file.choose()
train <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/train.csv") 

# replace with correct directory. Can use file.choose()
test <- read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/bigquery-geotab-intersection-congestion-data/test.csv") 



# temporarily rename Latitude/Longitude to lat/lon for tigris to work 

names(train)[names(train) == "Latitude"] <- "lat"
names(train)[names(train) == "Longitude"] <- "lon"

names(test)[names(test) == "Latitude"] <- "lat"
names(test)[names(test) == "Longitude"] <- "lon"


tic()

# attach geoid to train

train <- append_geoid(train, "block group")

# attach geoid to test

test <- append_geoid(test, "block group")

toc()



# rename lat/lon to original Latitude/Longitude

names(train)[names(train) == "lat"] <- "Latitude"
names(train)[names(train) == "lon"] <- "Longitude"

names(test)[names(test) == "lat"] <- "Latitude"
names(test)[names(test) == "lon"] <- "Longitude"


