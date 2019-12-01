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

## For setting up XGBoost
# install.packages("drat", repos="https://cran.rstudio.com")
# drat:::addRepo("dmlc")
# install.packages("xgboost", repos="http://dmlc.ml/drat/", type = "source")

library(xgboost)



load("backup_data_files/train_complete.RData")
load("backup_data_files/test_complete.RData")

submission <- read.csv("bigquery-geotab-intersection-congestion-data/sample_submission.csv")

source("cleanup.R")
source("random_forest.R")
source("linear_regression.R")
source("elastic_net.R")
source("logistic_hurdle.R")
source("xgboost.R")



modeling_plan <- drake_plan(
  
  responses = select(train_complete, starts_with("TotalTime"), starts_with("TimeFrom"),
                     starts_with("DistanceTo")),
  
  lin_reg_results = geotab_linear_regression(train_complete, test_complete, submission),
  submission_linear_regression = geotab_cleanup(lin_reg_results$submission_linear_regression, rounding = TRUE),
  
  elastic_net_results = geotab_elastic_net(train_complete, test_complete, submission),
  submission_elastic_net = geotab_cleanup(elastic_net_results$submission_elastic_net, rounding = TRUE),
  ft_select_results = en_feature_selection(elastic_net_results),
  train_en_select = ft_select_results$train_en_select,
  test_en_select = ft_select_results$test_en_select,
  
  rf_results = geotab_random_forest(train_complete, test_complete),
  imps = rf_results$imps,
  
  xgboost_results = geotab_xgboost(elastic_net_results$train_mat, responses, elastic_net_results$test_mat, submission),
  submission_xgboost = geotab_cleanup(xgboost_results$submission_xgboost)
  
)

config <- drake_config(modeling_plan)

vis_drake_graph(config)



make(modeling_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



# loadd(imps)
# save(imps, file = "imps.RData")

# loadd(submission_linear_regression)
# write.csv(submission_linear_regression, file = "submission_files/submission_linear_regression.csv", row.names = FALSE)

# loadd(train_en_select)
# save(train_en_select, file = "modeling_files/train_en_select.RData")

# loadd(test_en_select)
# save(test_en_select, file = "modeling_files/test_en_select.RData")

# loadd(submission_elastic_net)
# write.csv(submission_elastic_net, file = "submission_files/submission_elastic_net.csv", row.names = FALSE)

loadd(submission_xgboost)
write.csv(submission_xgboost, file = "submission_files/submission_xgboost.csv", row.names = FALSE)




# helpful code to deal with sparse matrix

# head(train_en_select[,!grepl("GeoIdTruncWithOther|IntersectionCityWithOther|EntryStreetNameWithOther|ExitStreetNameWithOther", colnames(train_en_select))])

# lm(train_complete$TotalTimeStopped_p50 ~ ., data = data.frame(as.matrix(train_en_select)))


