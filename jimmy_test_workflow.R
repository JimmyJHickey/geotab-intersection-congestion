source("logistic_hurdle.R")

load("data/train_en_select.RData")
load("data/test_en_select.RData")

load("backup_data_files/train_complete.RData")

train_complete = train_complete

train_df = as.data.frame(as.matrix(train_en_select))
test_df = as.data.frame(as.matrix(test_en_select))

test_complete = NULL

x = geotab_hurdle(train_predictors = train_df,
                  train_responses = train_complete,
                  test_predictors = test_complete)
