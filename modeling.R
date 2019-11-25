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
source("logistic_hurdle.R")



modeling_plan <- drake_plan(
  
  lin_reg_results = geotab_elastic_net(train_complete, test_complete, submission),
  submission_linear_regression = lin_reg_results$submission_linear_regression,
  
  elastic_net_results = geotab_elastic_net(train_complete, test_complete, submission),
  train_en_select = elastic_net_results$train_en_select,
  test_en_select = elastic_net_results$test_en_select,
  
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

loadd(train_en_select)
save(train_en_select, file = "modeling_files/train_en_select.RData")

loadd(test_en_select)
save(test_en_select, file = "modeling_files/test_en_select.RData")




# ad hoc code to circumvent Error: vector memory exhausted (limit reached?)

loadd(lin_reg_results)

vars_test = select(test_complete, -RowId, -IntersectionId, -Path, -ends_with("Raw"))



en_res <- lin_reg_results$en_res

selected <- coef(en_res)

sel <- as.numeric(selected[[2]])

sel_idx <- which(sel[-1] != 0)



train_mat <- lin_reg_results$train_mat

test_mat <- sparse.model.matrix(~ ., vars_test)[,-1]



# whoops--I forgot to make sure train_mat and test_mat had the same columns. Oh well-- train_en_select should be good tho

train_en_select_TotalTimeStopped_p50 <- train_mat[,sel_idx]
test_en_select_TotalTimeStopped_p50 <- test_mat[,sel_idx]



save(test_mat, file = "modeling_files/test_mat.RData")
save(train_en_select_TotalTimeStopped_p50, file = "modeling_files/train_en_select_TotalTimeStopped_p50.RData")
save(test_en_select_TotalTimeStopped_p50, file = "modeling_files/test_en_select_TotalTimeStopped_p50.RData")

# head(train_en_select_TotalTimeStopped_p50[,!grepl("GeoId|IntersectionCity|EntryStreetName|ExitStreetName", colnames(train_en_select_TotalTimeStopped_p50))])



