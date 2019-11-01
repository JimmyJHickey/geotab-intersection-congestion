
# later, put the whole drake plan in here

source("merge_distCBD.R")



# this script file will run thru the entire pipeline, doing all the merging of extra data and features to the original train and test

load("backup_data_files/train_append_weather.RData")

load("backup_data_files/test_append_weather.RData")



# sourcing in Zhen's merge_distCBD function

res = merge_distCBD(train_append_weather, test_append_weather)

train_merge_DistCBD = res$train_merge_DistCBD

test_merge_DistCBD = res$test_merge_DistCBD





# merge in Jimmy's feature engineering

