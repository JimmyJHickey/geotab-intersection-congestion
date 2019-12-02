
load("bst_list_11-30.RData")

imp_list <- list()



for (i in 1:length(bst_list)) {
  
  imp_list[[i]] <- xgb.importance(model = bst_list[[i]])
  
}

xgb.plot.importance(importance_matrix = imp_list[[1]], top_n = 50)

xgb.plot.importance(importance_matrix = imp_list[[2]], top_n = 50)

xgb.plot.importance(importance_matrix = imp_list[[3]], top_n = 50)



xgb.plot.importance(importance_matrix = imp_list[[4]], top_n = 50)

xgb.plot.importance(importance_matrix = imp_list[[5]], top_n = 50)

xgb.plot.importance(importance_matrix = imp_list[[6]], top_n = 50)


