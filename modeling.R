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
source("elastic_net.R")



modeling_plan <- drake_plan(
  
  lin_reg_results = geotab_linear_regression(train_complete, test_complete, submission),
  submission_linear_regression = lin_reg_results$submission_linear_regression,
  
  elastic_net_results = geotab_elastic_net(train_complete, test_complete, submission),
  ft_select_results = en_feature_selection(elastic_net_results),
  train_en_select = ft_select_results$train_en_select,
  test_en_select = ft_select_results$test_en_select,
  
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

# loadd(submission_linear_regression)
# write.csv(submission, file = "submission_files/submission_linear_regression.csv", row.names = FALSE)

# loadd(train_en_select)
# save(train_en_select, file = "modeling_files/train_en_select.RData")
# 
# loadd(test_en_select)
# save(test_en_select, file = "modeling_files/test_en_select.RData")



# helpful code to deal with sparse matrix

# head(train_en_select[,!grepl("GeoIdTruncWithOther|IntersectionCityWithOther|EntryStreetNameWithOther|ExitStreetNameWithOther", colnames(train_en_select))])

# lm(train_complete$TotalTimeStopped_p50 ~ ., data = data.frame(as.matrix(train_en_select)))


