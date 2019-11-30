
geotab_xgboost <- function(train, responses, test, submission_xgboost) {
  
  resp_idx <- c(1, 3, 5, 11, 13, 15)
  
  bst_list <- list()
  preds <- matrix(NA, nrow = dim(test)[1], ncol = 6)
  
  for (i in 1:6) {
    
    param <- list(lambda = 1, nthread = 4, booster = "gbtree", 
                  eval_metric = "rmse", objective = "reg:squarederror",
                  min_child_weight = 20, eta = 0.05, colsample_bytree = 0.6,
                  max.depth = 20, subsample = 0.8)
    
    bst <- xgboost(param, data = train, label = responses[[ resp_idx[i] ]], 
                   nrounds = 200)
    
    bst_list[[i]] <- bst
    
    preds[,i] <- predict(bst, test)
    
  }
  
  submission_xgboost$Target <- as.vector(t(preds))
  
  # return(list(train_mat = train_mat, test_mat = test_mat, en_cv = en_cv, en_res = en_res, 
  #             submission_elastic_net = submission_elastic_net)) 
  
}


