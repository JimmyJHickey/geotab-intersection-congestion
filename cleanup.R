# clean up responses for submission file

geotab_cleanup = function(submission,
                          rounding = FALSE)
{
  # make sure we have no negatives
  submission$Target = max(submission$Target, 0)
  
  # round to nearest integer
  
  if (rounding)
  {
    sumbission$Target = round(submission$Target)
  }
  
  
  return(submission)
}