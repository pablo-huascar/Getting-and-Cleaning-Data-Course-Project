# Getting-and-Cleaning-Data-Course-Project
## Load Packages and get the Data

library(readr)
library(tidyverse)

if(!file.exists("./data")){dir.create("./data")}

download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip",
              destfile = "./data/Dataset.zip")

unzip(zipfile = "./data/Dataset.zip", 
      exdir = "./data", 
      overwrite = TRUE)

## Load train datasets

features <- 
  read_table("data/UCI HAR Dataset/features.txt", col_names = c("cod",
                                                                "features"))

x_train <- 
  read_table("data/UCI HAR Dataset/train/X_train.txt", col_names = features$features)

y_train <- 
  read_table("data/UCI HAR Dataset/train/y_train.txt",
             col_names = "activity") 

subject_train <- 
  read_table("data/UCI HAR Dataset/train/subject_train.txt",
             col_names = "subject")

## merge train datasets

train <- 
  bind_cols(list(subject_train,
                 y_train,
                 x_train))

## Load test datasets 

x_test <- 
  read_table("data/UCI HAR Dataset/test/X_test.txt", 
             col_names = features$features)

y_test <- 
  read_table("data/UCI HAR Dataset/test/y_test.txt",
             col_names = "activity")

subject_test <- 
  read_table("data/UCI HAR Dataset/test/subject_test.txt",
             col_names = "subject")

## merge test datasets

test <- 
  bind_cols(list(subject_test,
                 y_test,
                 x_test))

# 1) Merges the training and the test sets to create one data set.

data_set <- 
  bind_rows(train,
            test)

# 2) Extracts only the measurements on the mean and standard deviation for each measurement.

data_set <- 
  data_set %>% 
  select(subject,
         activity, 
         contains(c("mean",
                    "std")))

# 3) Uses descriptive activity names to name the activities in the data set

data_set <- 
  data_set %>% 
  mutate(activity = recode(activity, 
                           "1" = "WALKING",
                           "2" = "WALKING_UPSTAIRS",
                           "3" = "WALKING_DOWNSTAIRS",
                           "4" = "SITTING",
                           "5" = "STANDING",
                           "6" = "LAYING"))

# 4) Appropriately labels the data set with descriptive variable names. 

data_set <- 
  data_set %>% 
  rename_with(~ gsub("Acc", "Accelerometer", .x)) %>% 
  rename_with(~ gsub("Gyro", "Gyroscope", .x)) %>% 
  rename_with(~ gsub("BodyBody", "Body", .x)) %>% 
  rename_with(~ gsub("Mag", "Magnitude", .x)) %>% 
  rename_with(~ gsub("^t", "Time", .x)) %>% 
  rename_with(~ gsub("^f", "Frequency", .x)) %>% 
  rename_with(~ gsub("tBody", "TimeBody", .x)) %>% 
  rename_with(~ gsub("-mean()", "Mean", .x)) %>%
  rename_with(~ gsub("-std()", "STD", .x)) %>%
  rename_with(~ gsub("-freq()", "Frequency", .x)) %>%
  rename_with(~ gsub("angle", "Angle", .x)) %>%
  rename_with(~ gsub("gravity", "Gravity", .x))

# 5) From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

tidy_data <- 
  data_set %>% 
  group_by(subject,
           activity) %>% 
  summarise(across(everything(), 
                   mean))

write.table(x = tidy_data,
            file = "tidy_data.txt",
            row.names = F)
## Creat a codebook
dataMaid::makeDataReport(data = tidy_data, 
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


