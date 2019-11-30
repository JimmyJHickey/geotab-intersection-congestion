
# KNN regression 2-fold cross-validation

#reference:
# http://www.sthda.com/english/articles/35-statistical-machine-learning-essentials/142-knn-k-nearest-neighbors-essentials/

require(caret)
require(tictoc)

# for local files

setwd("D:/Dropbox/ST790/Project")
setwd("E:/Project")

# load train set
load(file.path('data/train_complete.RData'))
train <- as.data.frame(train_complete)

# load test set
load(file.path('data/test_complete.RData'))
test <- as.data.frame(test_complete)

## predictors 
load(file.path('data/train_en_select.RData'))
x.train <- data.frame(as.matrix(train_en_select))

# load test set
load(file.path('data/test_en_select.RData'))
x.test <- data.frame(as.matrix(test_en_select))

# KNN.reg CV

#y.train
y.train <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, train$TotalTimeStopped_p80, 
                 train$DistanceToFirstStop_p20, train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)

train1 <- cbind(x.train, y.train[,1])
train2 <- cbind(x.train, y.train[,2])
train3 <- cbind(x.train, y.train[,3])
train4 <- cbind(x.train, y.train[,4])
train5 <- cbind(x.train, y.train[,5])
train6 <- cbind(x.train, y.train[,6])

#y.test 
y.test <- cbind(test$TotalTimeStopped_p20, test$TotalTimeStopped_p50, test$TotalTimeStopped_p80, 
                test$DistanceToFirstStop_p20, test$DistanceToFirstStop_p50, test$DistanceToFirstStop_p80)


# this is an example of knn.reg CV code from package("caret") 
knn1 <- train(train1$`y.train[, 1]` ~., data = train1, method = "knn",
              trControl = trainControl("cv", number = 2), #this is 2-fold CV#
              preProcess = c("center","scale"), 
              tuneLength = 900) ## tuneLength is the number of neighbors ,k

pred < predict(knn1, train1)

# this is small sample size 
train_s <- train1[1:500,]
x.test_s <- x.test[1:300, ]
knn2 <- train(y.train[, 1] ~., data = train_s, method = "knn",
              trControl = trainControl("cv", number = 2), #this is 2-fold CV#
              preProcess = c("center","scale"), 
              tuneLength = 50) ## tuneLength is the number of neighbors ,k

pred < predict(knn2, x.test_s)


#pred < knn%>% predict(train1)

# Tuning for best k   
## k = 926 is sqrt of sample size of train data
## k = 1414 is sqrt of sample size of train data

k <- seq(900, 1500, by=100)

pred_k1 <- pred_k2 <- pred_k3 <- pred_k4 <- pred_k5 <- pred_k6 <- pred_k7 <- matrix()

set.seed(123)

tic()
################################################################################################################
k <- k[1]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])
#y.test[,1]
pred_k1[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])
#y.test[,2]
pred_k1[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])

#y.test[,3]
pred_k1[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])
#y.test[,4]
pred_k1[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])
#y.test[,5]
pred_k1[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[1])
#y.test[,6]
pred_k1[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[2]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])
#y.test[,1]
pred_k2[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])
#y.test[,2]
pred_k2[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])

#y.test[,3]
pred_k2[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])
#y.test[,4]
pred_k2[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])
#y.test[,5]
pred_k2[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[2])
#y.test[,6]
pred_k2[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[3]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])
#y.test[,1]
pred_k3[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])
#y.test[,2]
pred_k3[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])

#y.test[,3]
pred_k3[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])
#y.test[,4]
pred_k3[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])
#y.test[,5]
pred_k3[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[3])
#y.test[,6]
pred_k3[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[4]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])
#y.test[,1]
pred_k4[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])
#y.test[,2]
pred_k4[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])

#y.test[,3]
pred_k4[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])
#y.test[,4]
pred_k4[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])
#y.test[,5]
pred_k4[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[4])
#y.test[,6]
pred_k4[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[5]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])
#y.test[,1]
pred_k5[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])
#y.test[,2]
pred_k5[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])

#y.test[,3]
pred_k5[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])
#y.test[,4]
pred_k5[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])
#y.test[,5]
pred_k5[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[5])
#y.test[,6]
pred_k5[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[6]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])
#y.test[,1]
pred_k6[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])
#y.test[,2]
pred_k6[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])

#y.test[,3]
pred_k6[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])
#y.test[,4]
pred_k6[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])
#y.test[,5]
pred_k6[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[6])
#y.test[,6]
pred_k6[,6] <- predict(knn, x.test)

################################################################################################################
k <- k[7]

#y.train[,1]
knn <- train(y.train[,1] ~., data = train1, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])
#y.test[,1]
pred_k7[,1] <- predict(knn, x.test)

#y.train[,2]
knn <- train(y.train[,2] ~., data = train2, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])
#y.test[,2]
pred_k7[,2] <- predict(knn, x.test)

#y.train[,3]
knn <- train(y.train[,3] ~., data = train3, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])

#y.test[,3]
pred_k7[,3] <- predict(knn, x.test)


#y.train[,4]
knn <- train(y.train[,4] ~., data = train4, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])
#y.test[,4]
pred_k7[,4] <- predict(knn, x.test)

#y.train[,5]
knn <- train(y.train[,5] ~., data = train5, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])
#y.test[,5]
pred_k7[,5] <- predict(knn, x.test)

#y.train[,6]
knn <- train(y.train[,6] ~., data = train6, method = "knn",
             trControl = trainControl("cv", number = 2), 
             preProcess = c("center","scale"), 
             tuneLength = k[7])
#y.test[,6]
pred_k7[,6] <- predict(knn, x.test)

toc()
