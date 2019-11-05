
library(randomForest)
library(drake)
library(dplyr)

# load dependencies

load("backup_data_files/train_complete.RData")
load("backup_data_files/test_complete.RData")

source("random_forest.R")



modeling_plan <- drake_plan(
  
  rf_results = geotab_random_forest(train_complete, test_complete),
  rf_total_time_50 = rf_results$rf_total_time_50,
  imps = rf_results$imps
  
)

config <- drake_config(modeling_plan)

vis_drake_graph(config)



make(modeling_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



loadd(imps)
save(imps, file = "imps.RData")




