###
# Jimmy Hickey
# 2019-11-26
# Clean up responses for submission file by changing negatives to 0.
# It also has the option to round
# 
# ================
# inputs
# ================
# sumbission: 
#   Predictions in submission format. It should have an ID column and a Target column.
#
# rounding:
#   Boolean that will round the responses. Default set to FALSE.
#
#
# ================
# outputs
# ================
# submission:
#   Same submission format with responses updated.
####


geotab_cleanup = function(submission,
                          rounding = FALSE)
{
  # make sure we have no negatives
  submission$Target = sapply(submission$Target, function(x) max(x, 0))
  
  # round to nearest integer
  if (rounding)
  {
    submission$Target = round(submission$Target)
  }
  
  
  return(submission)
}