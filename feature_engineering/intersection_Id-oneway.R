###
# Add unique intersection ID
# Jimmy Hickey
# 2019-30-10
###

library(dplyr)

train = read.csv("../data/train.csv")

test = read.csv("../data/test.csv")

train$IntersectionCity = paste( train$IntersectionId , "-", train$City)

test$IntersectionCity = paste( test$IntersectionId , "-", test$City)

write.csv(train, file = "../engineered_features/train-IDs.csv")
write.csv(test, file = "../engineered_features/test-IDs.csv")

write_oneway = function(
                    input_data,
                    output_file_entry,
                    output_file_exit
                  )
{
  
  # count number of directions entered and exited from each street
  entry = input_data %>% group_by(IntersectionCity, EntryStreetName) %>%
    summarize(count = n_distinct(EntryHeading))
  exit = input_data %>% group_by(IntersectionCity, ExitStreetName) %>%
    summarize(count = n_distinct(ExitHeading))
  
  
    # one way entries
  
    # if entered from one way or fewer, then this is a oneway street else it is not
    entry$is_oneway_entry = (entry$count <= 1) * 1
    remove = c("count")
    entry = entry[ , !(names(entry) %in% remove)]
    
    # join data with new boolean
    joined = input_data %>% left_join(entry)

    write.csv( joined %>% select(RowId, is_oneway_entry), output_file_entry, row.names=FALSE)
  
    # one way exits
    
    exit$is_oneway_exit = (exit$count <= 1) * 1
    remove = c("count")
    exit = exit[ , !(names(exit) %in% remove)]
    joined = input_data %>% left_join(exit)
    
    write.csv( joined %>% select(RowId, is_oneway_exit), output_file_exit, row.names=FALSE)
}


write_oneway(
  input_data = train,
  output_file_entry = "../engineered_features/train-entry-oneway.csv",
  output_file_exit = "../engineered_features/train-exit-oneway.csv"
 )



write_oneway(
  input_data = test,
  output_file_entry = "../engineered_features/test-entry-oneway.csv",
  output_file_exit = "../engineered_features/test-exit-oneway.csv"
)