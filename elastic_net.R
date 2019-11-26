geotab_elastic_net <- function(train, test, submission) {
  
  submission_elastic_net <- submission
  
  # cbind the response variables
  responses <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, 
                     train$TotalTimeStopped_p80, train$DistanceToFirstStop_p20, 
                     train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)
  
  colnames(responses) <- c("TotalTimeStopped_p20", "TotalTimeStopped_p50", 
                           "TotalTimeStopped_p80", "DistanceToFirstStop_p20", 
                           "DistanceToFirstStop_p50", "DistanceToFirstStop_p80")
  
  indep_vars_train = select(train, -RowId, -IntersectionId, -starts_with("TotalTime"), -starts_with("TimeFrom"),
                            -starts_with("DistanceTo"), -Path, -ends_with("Raw"), -GeoId, -GeoIdTrunc, -IntersectionCity, 
                            -EntryStreetName, -ExitStreetName, 
                            -GeoIdTruncWithOther, -IntersectionCityWithOther, -EntryStreetNameWithOther, -ExitStreetNameWithOther)
  vars_test = select(test, -RowId, -IntersectionId, -Path, -ends_with("Raw"), -GeoId, -GeoIdTrunc, -IntersectionCity, 
                     -EntryStreetName, -ExitStreetName, -GeoIdTruncWithOther, -IntersectionCityWithOther, -EntryStreetNameWithOther, 
                     -ExitStreetNameWithOther)
  
  # Preparing for glmnet
  train_mat <- sparse.model.matrix(~ ., indep_vars_train)[,-1]
  test_mat <- sparse.model.matrix(~ ., vars_test)[,-1]
  
  # making sure dummy variables align between train and test
  train_mat <- train_mat[,colnames(train_mat) %in% colnames(test_mat)]
  test_mat <- test_mat[,colnames(test_mat) %in% colnames(train_mat)]
  
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
  
  glmnet_pred <- predict(en_res, type = "response", newx = test_mat)
  
  glmnet_pred_reshape <- glmnet_pred[,,1] # get rid of extraneous dimension
  
  submission_elastic_net$Target <- as.vector(t(glmnet_pred_reshape))
  
  return(list(train_mat = train_mat, test_mat = test_mat, en_cv = en_cv, en_res = en_res, 
              submission_elastic_net = submission_elastic_net))  
  
}



# returns 6 train and 6 test sparse matrices (for the 6 responses) with elastic-net selected features

en_feature_selection <- function(elastic_net_results) {
  
  en_res <- elastic_net_results$en_res
  
  train_mat <- elastic_net_results$train_mat
  
  test_mat <- elastic_net_results$test_mat
  
  
  
  
  # construct sparse train and test sparse matrices with features selected by regular elastic net
  
  # as it turns out, the same predictors have been chosen for every response! However, different coefficient
  # values were used, so still good idea to fit 6 separate models for every response
  
  selected <- coef(en_res)
  
  # arbitrarily choose selected predictors for 2nd response--doesn't matter, as same predictors chosen
  # across the 6 responses
  
  sel <- as.numeric(selected[[2]])
  
  sel_idx <- which(sel[-1] != 0)
  
  train_en_select <- train_mat[,sel_idx]
  test_en_select <- test_mat[,sel_idx]
  
  
  
  return(list(train_en_select = train_en_select, test_en_select = test_en_select))
  
}
