# Load Packages and get the Data

library(readr)
library(tidyverse)

if(!file.exists("./data")){dir.create("./data")}

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "./data/Dataset.zip")

unzip(zipfile = "./data/Dataset.zip", 
      exdir = "./data", 
      overwrite = TRUE)


# Load train datasets and Appropriately labels the data set with descriptive variable names

features <- 
  read_table("data/UCI HAR Dataset/features.txt", col_names = c("cod",
                                                                "features"))

x_train <- 
  read_table("data/UCI HAR Dataset/train/X_train.txt", col_names = features$features)

# Uses descriptive activity names to name the activities in the data set

y_train <- 
  read_table("data/UCI HAR Dataset/train/y_train.txt",
             col_names = "activity") %>% 
  mutate(activity = recode(activity, 
                           "1" = "WALKING",
                           "2" = "WALKING_UPSTAIRS",
                           "3" = "WALKING_DOWNSTAIRS",
                           "4" = "SITTING",
                           "5" = "STANDING",
                           "6" = "LAYING"))

subject_train <- 
  read_table("data/UCI HAR Dataset/train/subject_train.txt",
             col_names = "subject")

# merge train datasets

train <- 
  bind_cols(list(subject_train,
                 y_train,
                 x_train))

# Load test datasets and Appropriately labels the data set with descriptive variable names

x_test <- 
  read_table("data/UCI HAR Dataset/test/X_test.txt", 
             col_names = features$features)

# Uses descriptive activity names to name the activities in the data set

y_test <- 
  read_table("data/UCI HAR Dataset/test/y_test.txt",
             col_names = "activity") %>% 
  mutate(activity = recode(activity, 
                           "1" = "WALKING",
                           "2" = "WALKING_UPSTAIRS",
                           "3" = "WALKING_DOWNSTAIRS",
                           "4" = "SITTING",
                           "5" = "STANDING",
                           "6" = "LAYING"))

subject_test <- 
  read_table("data/UCI HAR Dataset/test/subject_test.txt",
             col_names = "subject")

# merge train datasets

test <- 
  bind_cols(list(subject_test,
                 y_test,
                 x_test))

# Merges the training and the test sets to create one data set.

df <- 
  bind_rows(train,
            test)

# Appropriately labels the data set with descriptive variable names

names(df)<-gsub("Acc", "Accelerometer", names(df))
names(df)<-gsub("Gyro", "Gyroscope", names(df))
names(df)<-gsub("BodyBody", "Body", names(df))
names(df)<-gsub("Mag", "Magnitude", names(df))
names(df)<-gsub("^t", "Time", names(df))
names(df)<-gsub("^f", "Frequency", names(df))
names(df)<-gsub("tBody", "TimeBody", names(df))
names(df)<-gsub("-mean()", "Mean", names(df), ignore.case = TRUE)
names(df)<-gsub("-std()", "STD", names(df), ignore.case = TRUE)
names(df)<-gsub("-freq()", "Frequency", names(df), ignore.case = TRUE)
names(df)<-gsub("angle", "Angle", names(df))
names(df)<-gsub("gravity", "Gravity", names(df))


# Extracts only the measurements on the mean and standard deviation for each measurement. 

features_mean_sd <- 
  df %>% 
  select(contains(c("mean",
                    "std")))

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

step_5 <- 
  df %>% 
  group_by(subject,
           activity) %>% 
  summarise(across(everything(), 
                   mean))

write.table(x = step_5,
            file = "step_5.txt",
            row.names = F)

dataMaid::makeDataReport(data = df, 
                         mode = c("summarize",  "visualize", "check"), 
                         smartNum = FALSE, 
                         file = "codebook_FinalData.Rmd",      
                         replace = TRUE, 
                         checks = list(character = "showAllFactorLevels",
                                       factor = "showAllFactorLevels", 
                                       labelled = "showAllFactorLevels",
                                       haven_labelled = "showAllFactorLevels",
                                       numeric = NULL,
                                       integer = NULL,
                                       logical = NULL, Date = NULL), 
                         listChecks = FALSE,
                         maxProbVals = Inf,
                         codebook = TRUE, 
                         reportTitle = "Codebook for FinalData")

