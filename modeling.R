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
  submission_linear_regression = lin_reg_results$submission_linear_regression,
  
  rf_results = geotab_random_forest(train_complete, test_complete),
  imps = rf_results$imps
  
  
)

config <- drake_config(modeling_plan)

vis_drake_graph(config)



make(modeling_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



loadd(imps)
save(imps, file = "imps.RData")

loadd(submission_linear_regression)
write.csv(submission, file = "submission_files/submission_linear_regression.csv", row.names = FALSE)




