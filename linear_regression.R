
train <- train_merge_TurnAngle

test <- test_merge_TurnAngle
  






# cbind the response variables
responses = cbind(train_merge_TurnAngle$TotalTimeStopped_p20, train_merge_TurnAngle$TotalTimeStopped_p50, 
                   train_merge_TurnAngle$TotalTimeStopped_p80, train_merge_TurnAngle$DistanceToFirstStop_p20, 
                   train_merge_TurnAngle$DistanceToFirstStop_p50, train_merge_TurnAngle$DistanceToFirstStop_p80)



# Simple baseline linear regression model

base_lm = lm(responses ~ Latitude + Longitude + EntryHeading + ExitHeading
              + Hour + Weekend + Month, data = train)



# lm(cbind(Y1,Y2,Y3) ~ X1+X2+X3, data=somedata)

# total_time = ['TotalTimeStopped_p20',
#               'TotalTimeStopped_p50', 
#               'TotalTimeStopped_p80']
# 
# target_stopped = ['DistanceToFirstStop_p20',
#                   'DistanceToFirstStop_p50',
#                   'DistanceToFirstStop_p80']