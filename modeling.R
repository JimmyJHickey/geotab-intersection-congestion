
library(randomForest)
library(drake)
library(dplyr)

# load dependencies

train_complete <- readd(train_complete)
test_complete <- readd(test_complete)

source("random_forest.R")



modeling_plan <- drake_plan(
  
  rf_results <- geotab_random_forest(train_complete, test_complete)
  
)

config <- drake_config(modeling_plan)

vis_drake_graph(config)



make(modeling_plan)

history <- drake_history(analyze = TRUE)

cache <- drake_cache()



save(imps, file = "imps.RData")




