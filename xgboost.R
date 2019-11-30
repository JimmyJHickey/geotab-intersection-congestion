
geotab_xgboost <- function(train, responses, test, submission_xgboost) {
  
  resp_idx <- c(1, 3, 5, 11, 13, 15)
  
  bst_list <- list()
  preds <- matrix(NA, nrow = dim(test)[1], ncol = 6)
  
  for (i in 1:6) {
    
    bst <- xgboost(data = train, label = responses[[ resp_idx[i] ]],
                   eta = 1, # max.depth = 2, nthread = 2, # comment out this line to use default parameters
                   nrounds = 2, objective = "reg:linear")
    
    bst_list[[i]] <- bst
    
    preds[,i] <- predict(bst, test)
    
  }
  
  submission_xgboost$Target <- as.vector(t(preds))
  
  # return(list(train_mat = train_mat, test_mat = test_mat, en_cv = en_cv, en_res = en_res, 
  #             submission_elastic_net = submission_elastic_net)) 
  
}


