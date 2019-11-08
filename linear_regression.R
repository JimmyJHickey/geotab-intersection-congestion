
train <- train_complete

test <- test_complete




# cbind the response variables
responses <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, 
                   train$TotalTimeStopped_p80, train$DistanceToFirstStop_p20, 
                   train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)

colnames(responses) <- c("TotalTimeStopped_p20", "TotalTimeStopped_p50", 
                         "TotalTimeStopped_p80", "DistanceToFirstStop_p20", 
                         "DistanceToFirstStop_p50", "DistanceToFirstStop_p80")



# Simple baseline linear regression model (return?)

base_lm <- lm(responses ~ Latitude + Longitude + EntryHeading + ExitHeading
              + Hour + Weekend + Month, data = train)





indep_vars = select(train_complete, -RowId, -IntersectionId, -starts_with("TotalTime"), -starts_with("TimeFrom"),
                   -starts_with("DistanceTo"), -Path, -ends_with("Raw"))

# Preparing for glmnet

train_mat <- sparse.model.matrix(~ ., indep_vars)[,-1]



# elastic net regularization

en_cv <- cv.glmnet(train_mat, responses, alpha = .95, family = "mgaussian", standardize.response = FALSE)

# $lambda.min
# [1] 0.0007917862
# 
# $lambda.1se
# [1] 0.05717337

en_res <- glmnet(train_mat, responses, alpha = .95, lambda = en_cv$lambda.1se, 
                 family = "mgaussian", standardize.response = FALSE)



save(en_cv, file = "modeling_files/en_cv.RData")
save(en_res, file = "modeling_files/en_cv.RData")

