

geotab_linear_regression <- function(train, test, submission) {
  
  submission_linear_regression <- submission
  
  # cbind the response variables
  responses <- cbind(train$TotalTimeStopped_p20, train$TotalTimeStopped_p50, 
                     train$TotalTimeStopped_p80, train$DistanceToFirstStop_p20, 
                     train$DistanceToFirstStop_p50, train$DistanceToFirstStop_p80)
  
  colnames(responses) <- c("TotalTimeStopped_p20", "TotalTimeStopped_p50", 
                           "TotalTimeStopped_p80", "DistanceToFirstStop_p20", 
                           "DistanceToFirstStop_p50", "DistanceToFirstStop_p80")
  
  # Simple baseline linear regression model 
  
  base_lm <- lm(responses ~ Latitude + Longitude + EntryHeading + ExitHeading + 
                  Hour + Weekend + Month + City + TotalPopulationDensity + Humidity + 
                  Temperature + DistCBD + TurnAngle + OnewayEntry + OnewayExit, data = train)
  
  # "Latitude", "Longitude", "EntryHeading", "ExitHeading", 
  #   "Hour", "Weekend", "Month", "City", "TotalPopulationDensity", "Humidity", 
  #   "Temperature", "DistCBD", "TurnAngle", "OnewayEntry", "OnewayExit"
  
  
  
  test_subset_lm <- select(test, Latitude, Longitude, EntryHeading, ExitHeading, 
                           Hour, Weekend, Month, City, TotalPopulationDensity, Humidity, 
                           Temperature, DistCBD, TurnAngle, OnewayEntry, OnewayExit)
  
  pred_lm <- predict(base_lm, newdata = test_subset_lm)
  
  # putting the predictions in a new submission
  
  submission_linear_regression$Target <- as.vector(t(pred_lm))
  
  return(list(base_lm = base_lm, submission_linear_regression = submission_linear_regression))
  
}




