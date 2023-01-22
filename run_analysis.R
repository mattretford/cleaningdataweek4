run_analysis <- function(){
  
  # read in test and train datasets
  X_test <- read.table("data/test/X_test.txt") 
  Y_test <- read.table("data/test/Y_test.txt")
  subject_test <- read.table("data/test/subject_test.txt")
  
  X_train <- read.table("data/train/X_train.txt") 
  Y_train <- read.table("data/train/Y_train.txt")
  subject_train <- read.table("data/train/subject_train.txt")
  
  features <- read.table("features.txt")
  activity_labels <- read.table("activity_labels.txt")
  
  #Combine columns for datasets test and train 
  test_data <- cbind(Y_test, subject_test, X_test)
  train_data <- cbind(Y_train, subject_train, X_train)
  
  #merge test and train datasets
  merged_dataset <- rbind(test_data, train_data)

  #Adding columns names for activity, subject and features
  collumn_names <- rbind("activity","subject", features)
  colnames(merged_dataset) <- collumn_names$V2
  
  #Internal function to return the activity label based on the index provided
  returnActivityLabel <- function(x) {
    return(activity_labels[x,2])
  }

  #Vectorise function to replace activity indexes with labels
  vReturnActivityLabel  <- Vectorize(returnActivityLabel)
  merged_dataset$activity <- vReturnActivityLabel(merged_dataset$activity)

  #Filter out only columns relating to mean or standard deviation 
  filteredNames<-grep("mean|std", colnames(merged_dataset), ignore.case =T)
  filteredSet <- merged_dataset[,c(1,2,filteredNames)]
  
  #creates a new data set with the average of each variable for each activity and each subject
  NewTable <- aggregate(filteredSet[, 3:ncol(filteredSet)], list(filteredSet$activity, filteredSet$subject), mean)
  colnames(NewTable)[1] <- "activity"
  colnames(NewTable)[2] <- "subject"
  
  return(NewTable)
  
}


Write <- function(x){
  
  write.table(x, file = "AssignmentSubmission.txt", col.names = TRUE, row.name = FALSE) 
}

Read <- function(){
  
  tableRead <- read.table("AssignmentSubmission.txt", header = TRUE) 
  return(tableRead)
}
