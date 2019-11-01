
merge_distCBD <- function(train, test) {
  
  # load train set
  train_Atlanta <- train[train$City == "Atlanta",]
  train_Boston <- train[train$City == "Boston",]
  train_Chicago <- train[train$City == "Chicago",]
  train_Philadelphia <- train[train$City == "Philadelphia",]
  
  # load test set
  test_Atlanta <- test[test$City == "Atlanta",]
  test_Boston <- test[test$City == "Boston",]
  test_Chicago <- test[test$City == "Chicago",]
  test_Philadelphia <- test[test$City == "Philadelphia",]
  
  # Atlanta train & test, Financial center, add latitude & longitude & distance
  lat <- 33.756520 #latitude of CBD
  lot <- -84.379520 #longitude of CBD
  
  train_Atlanta_1at <- rep(lat, nrow(train_Atlanta))
  train_Atlanta_1ot <- rep(lot, nrow(train_Atlanta))
  
  test_Atlanta_1at <- rep(lat, nrow(test_Atlanta))
  test_Atlanta_1ot <- rep(lot, nrow(test_Atlanta))
  
  #distance from CBD
  train_Atlanta_distCBD <- sqrt((train_Atlanta$Latitude-train_Atlanta_1at)^2+(train_Atlanta$Longitude-train_Atlanta_1ot)^2)
  test_Atlanta_distCBD <- sqrt((test_Atlanta$Latitude-test_Atlanta_1at)^2+(test_Atlanta$Longitude-test_Atlanta_1ot)^2)
  
  #merge Latitude Longitude distance_CBD
  train_Atlanta_merge_distCBD <- cbind(train_Atlanta, train_Atlanta_distCBD)
  test_Atlanta_merge_distCBD <- cbind(test_Atlanta, test_Atlanta_distCBD)
  
  #m <- train_Atlanta_m[1:2,186:289]
  
  # Boston train & test, Financial center, add latitude & longitude & distance
  lat <- 42.3553 #latitude of CBD 
  lot <- -71.05528 #longitude of CBD
  
  train_Boston_1at <- rep(lat, nrow(train_Boston))
  train_Boston_1ot <- rep(lot, nrow(train_Boston))
  
  test_Boston_1at <- rep(lat, nrow(test_Boston))
  test_Boston_1ot <- rep(lot, nrow(test_Boston))
  
  #distance from CBD
  train_Boston_distCBD <- sqrt((train_Boston$Latitude-train_Boston_1at)^2+(train_Boston$Longitude-train_Boston_1ot)^2)
  test_Boston_distCBD <- sqrt((test_Boston$Latitude-test_Boston_1at)^2+(test_Boston$Longitude-test_Boston_1ot)^2)
  
  #merge latitude longitude distance_CBD
  train_Boston_merge_distCBD <- cbind(train_Boston, train_Boston_distCBD)
  test_Boston_merge_distCBD <- cbind(test_Boston, test_Boston_distCBD)
  
  
  # Chicago train & test, Financial center, add latitude & longitude & distance
  lat <- 41.88448 #latitude of CBD #Longitude of CBD
  lot <- -87.63252 #longitude of CBD
  
  train_Chicago_1at <- rep(lat, nrow(train_Chicago))
  train_Chicago_1ot <- rep(lot, nrow(train_Chicago))
  
  test_Chicago_1at <- rep(lat, nrow(test_Chicago))
  test_Chicago_1ot <- rep(lot, nrow(test_Chicago))
  
  #distance from CBD
  train_Chicago_distCBD <- sqrt((train_Chicago$Latitude-train_Chicago_1at)^2+(train_Chicago$Longitude-train_Chicago_1ot)^2)
  test_Chicago_distCBD <- sqrt((test_Chicago$Latitude-test_Chicago_1at)^2+(test_Chicago$Longitude-test_Chicago_1ot)^2)
  
  #merge latitude longitude distance_CBD
  train_Chicago_merge_distCBD <- cbind(train_Chicago, train_Chicago_distCBD)
  test_Chicago_merge_distCBD <- cbind(test_Chicago, test_Chicago_distCBD)
  
  
  # Philadelphia train & test, Financial center, add latitude & longitude & distance
  lat <- 39.95051  #latitude of CBD 
  lot <- -75.16039 #longitude of CBD
  
  train_Philadelphia_1at <- rep(lat, nrow(train_Philadelphia))
  train_Philadelphia_1ot <- rep(lot, nrow(train_Philadelphia))
  
  test_Philadelphia_1at <- rep(lat, nrow(test_Philadelphia))
  test_Philadelphia_1ot <- rep(lot, nrow(test_Philadelphia))
  
  #distance from CBD
  train_Philadelphia_distCBD <- sqrt((train_Philadelphia$Latitude-train_Philadelphia_1at)^2+(train_Philadelphia$Longitude-train_Philadelphia_1ot)^2)
  test_Philadelphia_distCBD <- sqrt((test_Philadelphia$Latitude-test_Philadelphia_1at)^2+(test_Philadelphia$Longitude-test_Philadelphia_1ot)^2)
  
  #merge latitude longitude distance_CBD
  train_Philadelphia_merge_distCBD <- cbind(train_Philadelphia, train_Philadelphia_distCBD)
  test_Philadelphia_merge_distCBD <- cbind(test_Philadelphia, test_Philadelphia_distCBD)
  
  names(train_Atlanta_merge_distCBD)[ncol(train_Atlanta_merge_distCBD)] <- c("DistCBD")
  names(train_Boston_merge_distCBD)[ncol(train_Boston_merge_distCBD)] <- c("DistCBD")
  names(train_Chicago_merge_distCBD)[ncol(train_Chicago_merge_distCBD)] <- c("DistCBD")
  names(train_Philadelphia_merge_distCBD)[ncol(train_Philadelphia_merge_distCBD)] <- c("DistCBD")
  
  names(test_Atlanta_merge_distCBD)[ncol(test_Atlanta_merge_distCBD)] <- c("DistCBD")
  names(test_Boston_merge_distCBD)[ncol(test_Boston_merge_distCBD)] <- c("DistCBD")
  names(test_Chicago_merge_distCBD)[ncol(test_Chicago_merge_distCBD)] <- c("DistCBD")
  names(test_Philadelphia_merge_distCBD)[ncol(test_Philadelphia_merge_distCBD)] <- c("DistCBD")
  
  # merge data for 4 cities 
  train_merge_distCBD <- rbind(train_Atlanta_merge_distCBD, train_Boston_merge_distCBD, train_Chicago_merge_distCBD, train_Philadelphia_merge_distCBD)
  test_merge_distCBD <- rbind(test_Atlanta_merge_distCBD, test_Boston_merge_distCBD, test_Chicago_merge_distCBD, test_Philadelphia_merge_distCBD)

  return(list(train_merge_DistCBD = train_merge_distCBD, test_merge_DistCBD = test_merge_distCBD))
  
}



