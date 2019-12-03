
# install.packages("devtools")
# library(devtools)
# install_github("ropensci/drake")
library(drake)

library(sf)
library(ggplot2)
library(sp)
library(tictoc)
library(tigris)
library(Hmisc)
library(dplyr)



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
  
  long_lat <- paste(train_city$Latitude, train_city$Longitude) # better identifier of the intersection than IntersectionID
  
  long_lat_missing_geoid <- unique(long_lat[which(is.na(train_city$GeoID))])
  
  for (ll in long_lat_missing_geoid) {
    
    # splitting on space
    lat <- as.numeric(strsplit(ll, " ")[[1]][1])
    lon <- as.numeric(strsplit(ll, " ")[[1]][2]) 
    
    GeoID_append <- append_geoid(data.frame("lat" = lat, "lon" = lon), "block group")[3]
    
    train_city$GeoID[long_lat == ll] <- as.numeric(GeoID_append)
    
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





### Atlanta ###

test_Atlanta_append_GeoID <- drake_plan(
  # Extract Atlanta intersections
  test_Atlanta = test[test$City == "Atlanta",],
  # append the GeoID to test_Atlanta
  test_Atlanta1 = my_append_geoid(test_Atlanta, GA_boundary_file),
  test_Atlanta2 = my_append_geoid2(test_Atlanta1)
)


### Boston ###

test_Boston_append_GeoID <- drake_plan(
  # Extract Boston intersections
  test_Boston = test[test$City == "Boston",],
  # append the GeoID to test_Boston
  test_Boston1 = my_append_geoid(test_Boston, MA_boundary_file),
  test_Boston2 = my_append_geoid2(test_Boston1)
)



### Chicago ###

test_Chicago_append_GeoID <- drake_plan(
  # Extract Chicago intersections
  test_Chicago = test[test$City == "Chicago",],
  # append the GeoID to test_Chicago
  test_Chicago1 = my_append_geoid(test_Chicago, IL_boundary_file),
  test_Chicago2 = my_append_geoid2(test_Chicago1)
)



### Philadelphia ###

test_Philadelphia_append_GeoID <- drake_plan(
  # Extract Philadelphia intersections
  test_Philadelphia = test[test$City == "Philadelphia",],
  # append the GeoID to test_Philadelphia
  test_Philadelphia1 = my_append_geoid(test_Philadelphia, PA_boundary_file),
  test_Philadelphia2 = my_append_geoid2(test_Philadelphia1)
)



#### Merging Land Area of each block group ####

merge_land_area <- function(train_city, state_boundary_file) {
  
  state_boundary <- st_read(state_boundary_file)
  
  GeoID <- as.numeric(as.character(state_boundary$GEOID))
  
  LandArea <- state_boundary$ALAND
  
  # to automatically merge in the geometry (coordinates of the block group boundary) do smth like 
  # cbind(state_boundary$GEOID, state_boundary$ALAND), (remember to rename GEOID and ALAND accordingly)
  train_city <- merge(train_city, cbind(GeoID, LandArea), by = "GeoID", all.x = TRUE) 
  
  return(train_city)
  
}



# append LandArea to each train_city
train_city_append_LandArea <- drake_plan(
  train_Atlanta3 = merge_land_area(train_Atlanta2, GA_boundary_file),
  train_Boston3 = merge_land_area(train_Boston2, MA_boundary_file),
  train_Chicago3 = merge_land_area(train_Chicago2, IL_boundary_file),
  train_Philadelphia3 = merge_land_area(train_Philadelphia2, PA_boundary_file)
)



# append LandArea to each test_city
test_city_append_LandArea <- drake_plan(
  test_Atlanta3 = merge_land_area(test_Atlanta2, GA_boundary_file),
  test_Boston3 = merge_land_area(test_Boston2, MA_boundary_file),
  test_Chicago3 = merge_land_area(test_Chicago2, IL_boundary_file),
  test_Philadelphia3 = merge_land_area(test_Philadelphia2, PA_boundary_file)
)



#### Merging in Census Data ####

merge_acs <- function(train_append_GeoID, acs_file, old_var_names, new_var_names) {
  
  GeoID_char <- as.character(acs_file$GIS.Join.Match.Code)
  
  # formatting the GeoID to the same as that in train_append_GeoID
  GeoID <- as.numeric(paste0(substring(GeoID_char, 2, 3), substring(GeoID_char, 5, 7), substring(GeoID_char, 9, 15)))
  
  train_append_census <- merge(train_append_GeoID, data.frame(GeoID, acs_file[old_var_names]), by = "GeoID", all.x = TRUE)
  
  names(train_append_census)[names(train_append_census) %in% old_var_names] <- new_var_names
  
  # drop columns if its name contains "Duplicate" or it has NAs
  
  column_drop <- c()
  
  for (i in 1:length(names(train_append_census))) {
    
    if (grepl("Duplicate", names(train_append_census)[i])) {column_drop <- append(column_drop, i)}
    else if (sum(is.na(train_append_census[,i])) != 0) {column_drop <- append(column_drop, i)}
    
  }
  
  train_append_census <- train_append_census[,-column_drop]
  
  #code TBD
  
  return(train_append_census)
  
}



names_edit <- function(old_var_names) {
  
  # regular expression cases
  # number first (time to go to work)
  # am vs pm
  
  new_var_names <- rep("", length(old_var_names))
  
  # taking care of variables named "Estimates..Total", "Estimates..Total.1",...
  new_var_names[old_var_names == "Estimates..Total"] <- "TotalPopulation"
  new_var_names[old_var_names == "Estimates..Total.1"] <- "TotalWorkers"
  new_var_names[old_var_names == "Estimates..Total.2"] <- "Duplicate" # this is the same as "Estimates..Total.1"
  new_var_names[old_var_names == "Estimates..Total.3"] <- "TotalWorkersNotWorkFromHome"
  new_var_names[old_var_names == "Estimates..Total.4"] <- "Duplicate.1" # this is the same as "Estimates..Total.3"
  new_var_names[old_var_names == "Estimates..Total.5"] <- "TotalPopulation16YrsAndOver"
  new_var_names[old_var_names == "Estimates..Total.6"] <- "TotalCivilianEmployed" # not subset nor superset of TotalWorkers, weirdly enough
  new_var_names[old_var_names == "Estimates..Total.7"] <- "TotalHousingUnits"
  
  # for the other old_var_names
  old_var_names_not_total <- old_var_names[!grepl("Estimates..Total", old_var_names)]
  
  old_var_names_shorten <- sub("Estimates..", "", old_var_names_not_total)
  
  var_name_split <- strsplit(old_var_names_shorten, "\\.+")
  
  var_name_split_capitalize <- lapply(var_name_split, capitalize)
  
  var_name_paste <- lapply(var_name_split_capitalize, paste, collapse = '')
  
  new_var_names_pre <- unlist(var_name_paste)
  
  new_var_names_empty_idx <- which(new_var_names == "")
  
  for (i in 1:length(new_var_names_pre)) {
    
    if (grepl("AM|PM", new_var_names_pre[i])) { # special case: time leaving home to go to work
      new_var_names[new_var_names_empty_idx[i]] <- paste0("TimeLeavingHome", new_var_names_pre[i])
    } else if (grepl("Minutes", new_var_names_pre[i])) { # special case: travel time to work 
      new_var_names[new_var_names_empty_idx[i]] <- paste0("TravelTime", new_var_names_pre[i])
    } else if (grepl("^Male$|^Female$", new_var_names_pre[i])) { # (^Male([.]\d)?$)|(^Female([.]\d)?$)
      new_var_names[new_var_names_empty_idx[i]] <- paste0(new_var_names_pre[i], "Workers")
    } else if (grepl("^Male1$|^Female1$", new_var_names_pre[i])) {
      new_var_names[new_var_names_empty_idx[i]] <- paste0(sub(1, "", new_var_names_pre[i]), "CivilianEmployed")
    } else { # normal case
      new_var_names[new_var_names_empty_idx[i]] <- new_var_names_pre[i]
    }
    
  }
  
  # concatenate "Raw" to the front of each variable to indicate that they're raw counts 
  # (except for Median household income, but I'll label it raw anyway)
  new_var_names <- unlist(lapply(new_var_names, paste0, "Raw"))
  
  return(new_var_names)
  
}



# append census data to train
train_append_census_plan <- drake_plan(
  acs_file = read.csv("/Users/Alvin/Documents/NCSU_Fall_2019/geotab-intersection-congestion/external_data/census_data_and_variable_definitions/census_data.csv",
                      header = TRUE, skip = 1),
  # variables 1-37 are geographic identifiers, variables 54-65 have too many NAs, and variables 184-330 are the margins of error
  old_var_names = names(acs_file)[c(38:53, 66:183)], # 134 variables after omitting the listed variables above
  new_var_names = names_edit(old_var_names),
  train_append_census = merge_acs(train_append_GeoID, acs_file, old_var_names, new_var_names)
)



# append census data to test
test_append_census_plan <- drake_plan(
  # variables 1-37 are geographic identifiers, variables 54-65 have too many NAs, and variables 184-330 are the margins of error
  test_append_census = merge_acs(test_append_GeoID, acs_file, old_var_names, new_var_names)
)



# normalize census variables accordingly:
# for counts of people/housing units, I divide by LandArea to get population/housing densities. 
# Accordingly, these variables will have "Density" appended to their names, instead of "Raw"

normalize_census <- function(train_append_census) {
  
  LandArea <- train_append_census$LandArea
  
  raw_var <- select(train_append_census, ends_with("Raw"))
  
  raw_var_names <- names(raw_var)
  
  train_normalize_census <- train_append_census
  
  for (j in 1:dim(raw_var)[2]) {
    
    train_normalize_census <- mutate(train_normalize_census, new_density_var = raw_var[,j] / LandArea)
    
    names(train_normalize_census)[names(train_normalize_census) == "new_density_var"] <- paste0(sub("Raw", "", raw_var_names[j]), "Density")
    
  }
  
  return(train_normalize_census)
  
}


# normalize the census variables and append it to train
train_normalize_census_plan <- drake_plan(
  
  train_normalize_census = normalize_census(train_append_census)
  
)



# normalize the census variables and append it to test
test_normalize_census_plan <- drake_plan(
  
  test_normalize_census = normalize_census(test_append_census)
  
)





merge_census <- rbind(
  kaggle_data_read,
  train_Atlanta_append_GeoID,
  train_Boston_append_GeoID,
  train_Chicago_append_GeoID,
  train_Philadelphia_append_GeoID,
  train_city_append_LandArea,
  drake_plan(
    train_append_GeoID = rbind.data.frame(train_Atlanta3, train_Boston3, train_Chicago3, train_Philadelphia3)
  ),
  train_append_census_plan, 
  train_normalize_census_plan,
  # repeating everything for the test set
  test_Atlanta_append_GeoID,
  test_Boston_append_GeoID,
  test_Chicago_append_GeoID,
  test_Philadelphia_append_GeoID,
  test_city_append_LandArea,
  drake_plan(
    test_append_GeoID = rbind.data.frame(test_Atlanta3, test_Boston3, test_Chicago3, test_Philadelphia3)
  ),
  test_append_census_plan, 
  test_normalize_census_plan
)





config <- drake_config(merge_census)

vis_drake_graph(config)




make(merge_census)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



# save final data in convenient rds/RData

loadd(train_normalize_census)

save(train_normalize_census, file = "backup_data_files/train_normalize_census.RData")

write.csv(train_normalize_census, file = "backup_data_files/train_normalize_census.csv")



loadd(test_normalize_census)

save(test_normalize_census, file = "backup_data_files/test_normalize_census.RData")

write.csv(test_normalize_census, file = "backup_data_files/test_normalize_census.csv")













