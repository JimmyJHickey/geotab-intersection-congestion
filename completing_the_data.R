
library(dplyr)
library(drake)

# this script file will run thru a pipeline, doing the merging of extra data and features to the original train and test

# load dependencies

source("merge_distCBD.R") # sourcing in Zhen's merge_distCBD function

load("backup_data_files/train_append_weather.RData")

load("backup_data_files/test_append_weather.RData")




# function to fix up train_complete and test_complete



last_modif <- function(train_complete_pre, test_complete_pre) {
  
  # rename GeoID -> GeoId
  names(train_complete_pre)[names(train_complete_pre) == "GeoID"] <- "GeoId"
  names(test_complete_pre)[names(test_complete_pre) == "GeoID"] <- "GeoId"
  
  # make variables categorical
  train_complete_pre$GeoId <- as.factor(train_complete_pre$GeoId)
  test_complete_pre$GeoId <- as.factor(test_complete_pre$GeoId)
  
  train_complete_pre$Weekend <- as.factor(train_complete_pre$Weekend)
  test_complete_pre$Weekend <- as.factor(test_complete_pre$Weekend)
  
  train_complete_pre$OnewayEntry <- as.factor(train_complete_pre$OnewayEntry)
  test_complete_pre$OnewayEntry <- as.factor(test_complete_pre$OnewayEntry)
  
  train_complete_pre$OnewayExit <- as.factor(train_complete_pre$OnewayExit)
  test_complete_pre$OnewayExit <- as.factor(test_complete_pre$OnewayExit) 
  
  # reorder IntersectionCity to be the third variable
  train_complete = select(train_complete_pre, RowId, GeoId, IntersectionCity, everything())
  test_complete = select(test_complete_pre, RowId, GeoId, IntersectionCity, everything())
  
  return(list(train_complete = train_complete, test_complete = test_complete))
  
}




# constructing the plan

complete_data_plan <- drake_plan(
  
  res = merge_distCBD(train_append_weather, test_append_weather),
  
  train_merge_DistCBD = res$train_merge_DistCBD,
  
  test_merge_DistCBD = res$test_merge_DistCBD,
  
  # merge in Jimmy's feature engineering
  
  # merge in turn angles 
  
  train_turn_angle = read.csv("feature_engineering/data_files/train_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle")),
  test_turn_angle = read.csv("feature_engineering/data_files/test_turn-angle.csv", header = FALSE, col.names = c("RowId", "TurnAngle")),
  
  train_merge_TurnAngle = merge(train_merge_DistCBD, train_turn_angle, by = "RowId"),
  test_merge_TurnAngle = merge(test_merge_DistCBD, test_turn_angle, by = "RowId"),
  
  ## merge in one-way indicator and city-IntersectionID 
  
  train_id = read.csv("feature_engineering/data_files/train-IDs.csv", header = TRUE),
  test_id = read.csv("feature_engineering/data_files/test-IDs.csv", header = TRUE),
  
  train_entry_oneway = read.csv("feature_engineering/data_files/train-entry-oneway.csv", header = TRUE),
  test_entry_oneway = read.csv("feature_engineering/data_files/test-entry-oneway.csv", header = TRUE),
  
  train_exit_oneway = read.csv("feature_engineering/data_files/train-exit-oneway.csv", header = TRUE),
  test_exit_oneway = read.csv("feature_engineering/data_files/test-exit-oneway.csv", header = TRUE),
  
  train_append = data.frame(train_id, OnewayEntry = train_entry_oneway$is_oneway_entry, 
                            OnewayExit = train_exit_oneway$is_oneway_exit),
  test_append = data.frame(test_id, OnewayEntry = test_entry_oneway$is_oneway_entry, 
                           OnewayExit = test_exit_oneway$is_oneway_exit),
  
  train_complete_pre = merge(train_merge_TurnAngle, train_append, by = "RowId"),
  test_complete_pre = merge(test_merge_TurnAngle, test_append, by = "RowId"),
  
  res2 = last_modif(train_complete_pre, test_complete_pre),
  
  train_complete = res2$train_complete,
  test_complete = res2$test_complete
  
)

config <- drake_config(complete_data_plan)

vis_drake_graph(config)



make(complete_data_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()


loadd(train_complete)
loadd(test_complete)

save(train_complete, file = "backup_data_files/train_complete.RData")
save(test_complete, file = "backup_data_files/test_complete.RData")






