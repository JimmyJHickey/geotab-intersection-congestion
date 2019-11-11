
geotab_random_forest <- function(train, test) {

# taking out variables I don't want RandomForest to use

# Raw variables are raw counts
# TotalTime, TimeFrom, DistanceTo are other response variables
# EntryStreetName and ExitStreetName and Path have too many categories
# RowId and IntersectionId should not be there

model_dat = select(train, -ends_with("Raw"), -starts_with("TotalTime"), -starts_with("TimeFrom"),
                   -starts_with("DistanceTo"), -ends_with("StreetName"), -Path, -RowId, -IntersectionId, -IntersectionCity)

sub_idx = sample(1:dim(train)[1], size = dim(train)[1] / 100)

# I'm using TotalTimeStopped as the response variable for the Random Forest
rf_total_time_50 = randomForest(train$TotalTimeStopped_p50[sub_idx] ~ ., data = model_dat[sub_idx,], importance = TRUE)

imps = importance(rf_total_time_50)   

# this plot is too messy--we have too many variables
# varImpPlot(rf_total_time_50) 

return(list(rf_total_time_50 = rf_total_time_50, imps = imps))

}

