run_analysis <- function(){
  ## Create the "Activities" dataset with well defined column names
  activities <- read.table("./UCI HAR Dataset/activity_labels.txt", header = FALSE)
  names(activities) <- c("activity_ID","activity_Name")
  
  ## Create the "Features" dataset with well defined column names
  features <- read.table("./UCI HAR Dataset/features.txt", header = FALSE)
  names(features) <- c("feature_ID","feature_Name")
  
  ## Create the "Features Subset" dataset to extract only columns with mean() and std() substrings
  features_Subset <- rbind(subset(features, grepl("mean()", features$feature_Name, fixed = TRUE)),
  subset(features, grepl("std()", features$feature_Name, fixed = TRUE)))
  
  ## Order the Subset on Feature_ID to get the column indices in order
  features_Subset <- features_Subset[order(features_Subset$feature_ID),]
  
  
  ## -----------------Start with the Test Data--------------------------
  ## Create the "Subject Test" dataset with well defined column names
  subTest <- read.table("./UCI HAR Dataset/test/subject_test.txt", header = FALSE)
  names(subTest) <- c("subject_ID")
  
  ## Create the "Observation Test" dataset with column names per the Features dataset
  xTest <- read.table("./UCI HAR Dataset/test/X_test.txt", header = FALSE)
  names(xTest) <- features$feature_Name
  
  ## Create the "Observation Test Subset" dataset to extract only columns with mean() and std() substrings
  xTest <- xTest[,features_Subset$feature_ID]
  
  ## Create the "Activity Test" dataset with well defined column names
  yTest <- read.table("./UCI HAR Dataset/test/y_test.txt", header = FALSE)
  names(yTest) <- c("activity_ID")
  
  ## Merge the "Activity Test" dataset with "Activities" dataset to get "Activity Test Names" dataset
  yTest_Complete <- merge(yTest,activities,by.x = "activity_ID",by.y = "activity_ID", all = TRUE)
  
  ## Bind the "Subject Test", "Activity Test Names" and "Observation Test Subset" dataset to create the "Test Dataset"
  testData <- cbind(subTest, yTest_Complete, xTest)
  
  
  ## -----------------Start with the Train Data--------------------------
  ## Create the "Subject Train" dataset with well defined column names
  subTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt", header = FALSE)
  names(subTrain) <- c("subject_ID")
  
  ## Create the "Observation Train" dataset with column names per the Features dataset
  xTrain <- read.table("./UCI HAR Dataset/train/X_train.txt", header = FALSE)
  names(xTrain) <- features$feature_Name
  
  ## Create the "Observation Train Subset" dataset to extract only columns with mean() and std() substrings
  xTrain <- xTrain[,features_Subset$feature_ID]
  
  ## Create the "Activity Train" dataset with well defined column names
  yTrain <- read.table("./UCI HAR Dataset/train/y_train.txt", header = FALSE)
  names(yTrain) <- c("activity_ID")
  
  ## Merge the "Activity Train" dataset with "Activities" dataset to get "Activity Train Names" dataset
  yTrain_Complete <- merge(yTrain,activities,by.x = "activity_ID",by.y = "activity_ID", all = TRUE)
  
  ## Bind the "Subject Train", "Activity Train Names" and "Observation Train Subset" dataset to create the "Train Dataset"
  trainData <- cbind(subTrain, yTrain_Complete, xTrain)
  
  ## --------------------Merge the Test & Train Dataset-----------------------
  tidy_Dataset1 <- rbind(testData, trainData)
  
  ## Check, install and load dplyr package
  if("dplyr" %in% rownames(installed.packages()) == FALSE) { install.packages("dplyr")}
  library(dplyr)
  
  ## Create a second, independent tidy data set with the average of each variable for each activity and each subject
  tidy_Dataset2 <- tidy_Dataset1 %>% group_by(subject_ID, activity_ID, activity_Name) %>% summarise_each(funs(mean))
  
  ## Write the Tidy datasets to txt files
  write.table(tidy_Dataset1, "UCI_HAR_Tidy_Dataset_1.txt", sep = ",", row.names = FALSE)
  write.table(tidy_Dataset2, "UCI_HAR_Tidy_Dataset_2.txt", sep = ",", row.names = FALSE)
}