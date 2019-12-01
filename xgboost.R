
geotab_xgboost <- function(train, responses, test, submission_xgboost) {
  
  resp_idx <- c(1, 3, 5, 11, 13, 15)
  
  # make training and validation sets from the original train
  set.seed(1014)
  
  train_idx <- sample(1:nrow(train), size = round(dim(train)[1] * .80), replace = FALSE)
  
  X_train <- train[train_idx,]
  Y_train <- responses[train_idx,]
  
  X_val <- train[-train_idx,]
  Y_val <- responses[-train_idx,]
  
  
  
  # XGBoost model for each of the six responses
  
  bst_list <- list()
  preds <- matrix(NA, nrow = dim(test)[1], ncol = 6)
  
  # saving the output of the xgboost models
  
  con <- file("xgb.log")
  sink(con, append=TRUE)
  sink(con, append=TRUE, type="message")
  
  for (i in 1:6) {
    
    dtrain <- xgb.DMatrix(data = X_train, label = Y_train[[ resp_idx[i] ]])
    dval <- xgb.DMatrix(data = X_val, label = Y_val[[ resp_idx[i] ]])
    
    param <- list(lambda = 1, nthread = 4, booster = "gbtree", 
                  eval_metric = "rmse", # objective = "reg:squarederror", # apparently this objective throws an error
                  min_child_weight = 20, eta = 0.05, colsample_bytree = 1,
                  max.depth = 20, subsample = 0.8)
    
    watchlist <- list(train = dtrain, validation = dval)
    
    bst <- xgb.train(param, dtrain, nrounds = 1000, watchlist = watchlist, early_stopping_rounds = 50)
    
    bst_list[[i]] <- bst
    
    preds[,i] <- predict(bst, test)
    
  }
  
  # end the sinking
  
  sink()
  sink(type="message")
  
  
  
  # formatting the predictions for submission
  
  submission_xgboost$Target <- as.vector(t(preds))
  
  return(list(bst_list = bst_list, submission_xgboost = submission_xgboost))
  
}


