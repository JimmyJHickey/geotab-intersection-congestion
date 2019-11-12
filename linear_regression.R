geotab_linear_regression <- function(train, test, submission) {
  
  submission_linear_regression <- submission
  submission_elastic_net <- submission
  
  # cbind the response variables
  responses <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, 
                     train$TotalTimeStopped_p80, train$DistanceToFirstStop_p20, 
                     train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)
  
  colnames(responses) <- c("TotalTimeStopped_p20", "TotalTimeStopped_p50", 
                           "TotalTimeStopped_p80", "DistanceToFirstStop_p20", 
                           "DistanceToFirstStop_p50", "DistanceToFirstStop_p80")
  
  # Simple baseline linear regression model 
  
  base_lm <- lm(responses ~ Latitude + Longitude + EntryHeading + ExitHeading + 
                  Hour + Weekend + Month + City + TotalPopulationDensity + Humidity + 
                  Temperature + DistCBD + TurnAngle + OnewayEntry + OnewayExit, data = train)
  
  # "Latitude", "Longitude", "EntryHeading", "ExitHeading", 
  #   "Hour", "Weekend", "Month", "City", "TotalPopulationDensity", "Humidity", 
  #   "Temperature", "DistCBD", "TurnAngle", "OnewayEntry", "OnewayExit"
  
  
  
  test_subset_lm <- select(test, Latitude, Longitude, EntryHeading, ExitHeading, 
                           Hour, Weekend, Month, City, TotalPopulationDensity, Humidity, 
                           Temperature, DistCBD, TurnAngle, OnewayEntry, OnewayExit)
  
  pred_lm <- predict(base_lm, newdata = test_subset_lm)
  
  # putting in the predictions in new sub
  
  submission_linear_regression$Target <- as.vector(t(pred_lm))
  
  
  
  # Elastic Net
  
  indep_vars = select(train, -RowId, -IntersectionId, -starts_with("TotalTime"), -starts_with("TimeFrom"),
                      -starts_with("DistanceTo"), -Path, -ends_with("Raw"))
  
  # Preparing for glmnet
  
  train_mat <- sparse.model.matrix(~ ., indep_vars)[,-1]
  
  # elastic net regularization
  
  en_cv <- cv.glmnet(train_mat, responses, alpha = .95, family = "mgaussian", standardize.response = FALSE)
  
  # # outdated
  # $lambda.min
  # [1] 0.0007917862
  # 
  # $lambda.1se
  # [1] 0.05717337
  
  en_res <- glmnet(train_mat, responses, alpha = .95, lambda = en_cv$lambda.1se, 
                   family = "mgaussian", standardize.response = FALSE)
  
  # constructing test_mat for prediction
  
  
  
  return(list(base_lm = base_lm, submission_linear_regression = submission_linear_regression, 
              train_mat = train_mat, en_cv = en_cv, en_res = en_res))
  
}

# # move into modeling.R ! 
# write.csv(submission, file = "submission_files/submission.csv", row.names = FALSE)
# 
# save(en_cv, file = "modeling_files/en_cv.RData")
# save(en_res, file = "modeling_files/en_cv.RData")



