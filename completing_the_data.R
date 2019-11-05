# this script file will run thru the entire pipeline, doing all the merging of extra data and features to the original train and test

# later, put the whole drake plan in here

source("merge_distCBD.R")

load("backup_data_files/train_append_weather.RData")

load("backup_data_files/test_append_weather.RData")



# sourcing in Zhen's merge_distCBD function

res = merge_distCBD(train_append_weather, test_append_weather)

train_merge_DistCBD = res$train_merge_DistCBD

test_merge_DistCBD = res$test_merge_DistCBD



# merge in Jimmy's feature engineering

## merge in turn angles 

train_turn_angle = read.csv("feature_engineering/data_files/train_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle"))
test_turn_angle = read.csv("feature_engineering/data_files/test_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle"))

train_merge_TurnAngle = merge(train_merge_DistCBD, train_turn_angle, by = "RowId")
test_merge_TurnAngle = merge(test_merge_DistCBD, test_turn_angle, by = "RowId")



## merge in one-way indicator and city-IntersectionID 

train_id = read.csv("feature_engineering/data_files/train-IDs.csv", header = TRUE)
test_id = read.csv("feature_engineering/data_files/test-IDs.csv", header = TRUE)

train_entry_oneway = read.csv("feature_engineering/data_files/train-entry-oneway.csv", header = TRUE)
test_entry_oneway = read.csv("feature_engineering/data_files/test-entry-oneway.csv", header = TRUE)

train_exit_oneway = read.csv("feature_engineering/data_files/train-exit-oneway.csv", header = TRUE)
test_exit_oneway = read.csv("feature_engineering/data_files/test-exit-oneway.csv", header = TRUE)

train_append = data.frame(train_id, OnewayEntry = train_entry_oneway$is_oneway_entry, 
                          OnewayExit = train_exit_oneway$is_oneway_exit)
test_append = data.frame(test_id, OnewayEntry = test_entry_oneway$is_oneway_entry, 
                          OnewayExit = test_exit_oneway$is_oneway_exit)

train_complete = merge(train_merge_TurnAngle, train_append, by = "RowId")
test_complete = merge(test_merge_TurnAngle, test_append, by = "RowId")

names(train_complete)[names(train_complete) == "GeoID"] <- "GeoId"
names(test_complete)[names(test_complete) == "GeoID"] <- "GeoId"

train_complete = select(train_complete, RowId, GeoId, IntersectionCity, everything())
test_complete = select(test_complete, RowId, GeoId, IntersectionCity, everything())



save(train_complete, file = "backup_data_files/train_complete.RData")
save(test_complete, file = "backup_data_files/test_complete.RData")






