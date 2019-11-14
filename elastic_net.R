geotab_elastic_net <- function(train, test, submission) {
  
  submission_elastic_net <- submission
  
  # cbind the response variables
  responses <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, 
                     train$TotalTimeStopped_p80, train$DistanceToFirstStop_p20, 
                     train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)
  
  colnames(responses) <- c("TotalTimeStopped_p20", "TotalTimeStopped_p50", 
                           "TotalTimeStopped_p80", "DistanceToFirstStop_p20", 
                           "DistanceToFirstStop_p50", "DistanceToFirstStop_p80")
  
  # Elastic Net
  
  indep_vars_train = select(train, -RowId, -IntersectionId, -starts_with("TotalTime"), -starts_with("TimeFrom"),
                            -starts_with("DistanceTo"), -Path, -ends_with("Raw"))
  vars_test = select(test, -RowId, -IntersectionId, -Path, -ends_with("Raw"))
  
  # Preparing for glmnet
  
  train_mat <- sparse.model.matrix(~ ., indep_vars_train)[,-1]
  test_mat <- sparse.model.matrix(~ ., vars_test)[,-1]
  
  train_mat <- train_mat[,colnames(train_mat) %in% colnames(test_mat)]
  
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
  
  # construct sparse train and test sparse matrices with features selected by regular elastic net
  
  selected <- coef(en_res)
  
  train_en_select <- list()
  test_en_select <- list()
  
  for (i in 1:length(selected)) {
    
    sel <- as.numeric(selected[[i]])
    
    sel_idx <- which(sel[-1] != 0)
    
    train_en_select[[i]] <- train_mat[,sel_idx]
    test_en_select[[i]] <- test_mat[,sel_idx]
    
  }
  
  return(list(train_mat = train_mat, test_mat = test_mat, en_cv = en_cv, en_res = en_res, 
              train_en_select = train_en_select, test_en_select = test_en_select))  
  
  
}

