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

train_turn_angle = read.csv("feature_engineering/train_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle"))
test_turn_angle = read.csv("feature_engineering/test_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle"))

train_merge_TurnAngle = merge(train_merge_DistCBD, train_turn_angle, by = "RowId")
test_merge_TurnAngle = merge(test_merge_DistCBD, test_turn_angle, by = "RowId")



