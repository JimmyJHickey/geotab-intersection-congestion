
library(dplyr)
library(drake)
library(forcats)

# this script file will run thru a pipeline, doing the merging of extra data and features to the original train and test

# load dependencies

source("merge_distCBD.R") # sourcing in Zhen's merge_distCBD function

load("backup_data_files/train_append_weather.RData")

load("backup_data_files/test_append_weather.RData")




# functions to fix up train_complete and test_complete



# # helper function to group rare categories to "Other" to reduce total number of levels
# # returns new variable with regrouped categories
# 
# group_rare_cats <- function(fct_var, desired_min_cat) {
#   
#   # Group rare fct_var levels into "Other", such that there is at least desired_min_cat values
#   # in each category
#   num_cats <- length(unique(fct_var))
#   min_cat <- 0 # initial value less than desired_min_cat
#   
#   while(min_cat < desired_min_cat & num_cats >= 0) {
#     num_cats <- num_cats - 1
#     tab <- table(fct_lump(fct_var, n = num_cats))
#     min_cat <- min(tab)
#   }
#   
#   return(fct_lump(fct_var, n = num_cats))
#   
# }



last_modif <- function(train_complete_pre, test_complete_pre) {
  
  # rename GeoID -> GeoId
  names(train_complete_pre)[names(train_complete_pre) == "GeoID"] <- "GeoId"
  names(test_complete_pre)[names(test_complete_pre) == "GeoID"] <- "GeoId"
  
  # truncate last three numbers from GeoId, to refer to broader category, census tracts
  train_complete_pre$GeoIdTrunc <- substr(train_complete_pre$GeoId, 1, 9)
  test_complete_pre$GeoIdTrunc <- substr(test_complete_pre$GeoId, 1, 9)
  
  # make variables categorical
  train_complete_pre$GeoId <- as.factor(train_complete_pre$GeoId)
  test_complete_pre$GeoId <- as.factor(test_complete_pre$GeoId)
  
  train_complete_pre$GeoIdTrunc <- as.factor(train_complete_pre$GeoIdTrunc)
  test_complete_pre$GeoIdTrunc <- as.factor(test_complete_pre$GeoIdTrunc)
  
  train_complete_pre$Weekend <- as.factor(train_complete_pre$Weekend)
  test_complete_pre$Weekend <- as.factor(test_complete_pre$Weekend)
  
  train_complete_pre$OnewayEntry <- as.factor(train_complete_pre$OnewayEntry)
  test_complete_pre$OnewayEntry <- as.factor(test_complete_pre$OnewayEntry)
  
  train_complete_pre$OnewayExit <- as.factor(train_complete_pre$OnewayExit)
  test_complete_pre$OnewayExit <- as.factor(test_complete_pre$OnewayExit) 
  
  # reorder IntersectionCity to be the third variable
  train_complete = select(train_complete_pre, RowId, GeoId, IntersectionCity, everything())
  test_complete = select(test_complete_pre, RowId, GeoId, IntersectionCity, everything())
  
  # relevel factors so that the base level is present in both train and test
  train_complete$IntersectionCity <- relevel(train_complete$IntersectionCity, "0 Boston")
  
  # Group rare categories to "Other" to reduce total number of levels
  # I modulate n depending on how many unique levels the variable has
  # I just repeat variables with test because the feature selection will automatically align
  # variables between train and test
  
  train_complete$GeoIdTruncWithOther <- fct_lump(train_complete$GeoIdTrunc, n = 200)
  test_complete$GeoIdTruncWithOther <- test_complete$GeoIdTrunc
  
  train_complete$IntersectionCityWithOther <- fct_lump(train_complete$IntersectionCity, n = 500)
  test_complete$IntersectionCityWithOther <- test_complete$IntersectionCity
  
  train_complete$EntryStreetNameWithOther <- fct_lump(train_complete$EntryStreetName, n = 300)
  test_complete$EntryStreetNameWithOther <- test_complete$EntryStreetName
  
  train_complete$ExitStreetNameWithOther <- fct_lump(train_complete$ExitStreetName, n = 300)
  test_complete$ExitStreetNameWithOther <- test_complete$ExitStreetName
  
  # Concatenate EntryHeading and ExitHeading
  train_complete$EntryExitHeading <- paste(train_complete$EntryHeading, train_complete$ExitHeading)
  test_complete$EntryExitHeading <- paste(test_complete$EntryHeading, test_complete$ExitHeading)
  
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






