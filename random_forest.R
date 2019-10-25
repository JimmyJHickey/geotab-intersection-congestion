
library(randomForest)
library(drake)
library(dplyr)

train_normalize_census <- readd(train_normalize_census)

# taking out variables I don't want RandomForest to use

# Raw variables are raw counts
# TotalTime, TimeFrom, DistanceTo are other response variables
# EntryStreetName and ExitStreetName and Path have too many categories
# RowId and IntersectionId should not be there

model_dat <- select(train_normalize_census, -ends_with("Raw"), -starts_with("TotalTime"), -starts_with("TimeFrom"), -starts_with("DistanceTo"), -ends_with("Name"), -Path, -RowId, -IntersectionId)

set.seed(452)

sub_idx <- sample(1:dim(train_normalize_census)[1], size = dim(train_normalize_census)[1] / 1000)

# I'm using TotalTimeStopped as the response variable for the Random Forest
rf_total_time_50 <- randomForest(train_normalize_census$TotalTimeStopped_p50[sub_idx] ~ ., data = model_dat[sub_idx,], importance = TRUE)




imps <- importance(rf_total_time_50)        
varImpPlot(rf_total_time_50) 



save(imps, "imps.RData")
