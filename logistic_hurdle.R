###
# Jimmy Hickey
# 2019-11-16
# A hurdle to predict some of the zeros 
# in the 80th percentile column.
# Then we can predict the others separately.
###

geotab_hurdle <- function(train_predictors, 
                          train_responses,
                          test_predictors) {

  training_data = train_predictors
  # testing_data = test
  

  # Make indicator for 80th percentile rows that are 0
  # If the 80th percentile of time stopped is 0, so are the other percentiles we need to predict.
  train_responses$zero_time_stopped = (train_responses$TotalTimeStopped_p80 == 0 ) * 1
  # testing_data$zero_time_stopped = (testing_data$TotalTimeStopped_p80 < 0) * 1

  head( train_responses$zero_time_stopped)
  
  logistic_zero_time_stopped = glm(train_responses$zero_time_stopped ~ ., data = training_data,
                                   family = binomial)
  
  # pred_lm <- predict(logistic_zero_time_stopped, newdata = testing_data)

  return(1)
  
}
# 
# x = geotab_hurdle(train_predictors = train_df,
#                   train_responses = train_complete,
#                   test_predictors = test_complete)