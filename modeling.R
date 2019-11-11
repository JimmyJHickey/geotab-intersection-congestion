# load dependencies

# If it's your first time using drake, use below code
# library(devtools)
# install_github("ropensci/drake")
library(drake)
library(dplyr)
library(Matrix)

# modeling packages
library(glmnet)
library(randomForest)

load("backup_data_files/train_complete.RData")
load("backup_data_files/test_complete.RData")

submission <- read.csv("bigquery-geotab-intersection-congestion-data/sample_submission.csv")

source("random_forest.R")
source("linear_regression.R")



modeling_plan <- drake_plan(
  
  lin_reg_results = geotab_linear_regression(train_complete, test_complete, submission),
  
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




