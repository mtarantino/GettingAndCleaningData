# Johns Hopkins Course : Getting and Cleaning Data
# Course Project : Human Activity Recognition Using Smartphones 

# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

library(data.table)
library(dplyr)

# Global Variables
DATASET_DIR <- "UCI HAR Dataset"
TEST_SET <- "test"
TRAIN_SET <- "train"
INTERTIAL_SIGNALS_DIR <- "Inertial Signals"

ACTIVITY_LABELS_FILE <- "activity_labels.txt"
FEATURES_VAR_NAME_FILE <- "features.txt"
SUBJECT_DATA_FILE_PREFIX <- "subject_"
FEATURES_FILE_PREFIX <- "X_"
ACTIVITY_FILE_PREFIX <- "y_"
SIGNAL_BODY_PREFIX <- "body_"
SIGNAL_TOTAL_PREFIX <- "total_"
DEVICE_GYRO_PREFIX <- "gyro_"
DEVICE_ACC_PREFIX <- "acc_"



# get_activity_labels()
# Read the file containing the activity name and return a dataframe containing 
# the id (activity_id) and activiry label (activity)
# Input :
# Return : data.frame
get_activity_labels <- function() {
    ds_activity_labels = fread(file.path(DATASET_DIR, ACTIVITY_LABELS_FILE),
                            header=FALSE, sep=" ", na.strings="NA", 
                            colClasses=c("integer", "character"),
                            col.names = c("activity_id","activity"))
    ds_activity_labels
}

# get_features_var_name()
# Read the file containing the list of all features and return a dataframe with 
# the id (feature_var_id) and feature name (feature_var_name)
# Input :
# Return : data.frame
get_features_var_name <- function() {
    ds_features_var_name = fread(file.path(DATASET_DIR, FEATURES_VAR_NAME_FILE),
                               header=FALSE, sep=" ", na.strings="NA", 
                               colClasses=c("integer", "character"),
                               col.names = c("feature_var_id","feature_var_name"))
    ds_features_var_name
}

# get_subject_data(subject_set)
# Read the subject data file containing the id of the subjects and return a datrafame
# with the suject id (subject_id)
# Input : 
#   subject_set : name of the subject set (e.g. test, train)
# Return : data.frame
get_subject_data <- function(subject_set) {
    ds_subject_data = fread(file.path(DATASET_DIR, subject_set, 
                               paste(SUBJECT_DATA_FILE_PREFIX,subject_set,".txt", sep="")),
                               header=FALSE, sep=" ", na.strings="NA", 
                               colClasses=c("integer"),
                               col.names = c("subject_id"))
    ds_subject_data
}

# get_activity_data(subject_set)
# Read the activity data file containing the id of activities and return a datrafame
# with the activity (activity_id)
# is specified in input
# Input : 
#   subject_set : name of the subject set (e.g. test, train)
# Return : data.frame
get_activity_data <- function(subject_set) {
    ds_activity_data = fread(file.path(DATASET_DIR, subject_set, 
                            paste(ACTIVITY_FILE_PREFIX,subject_set,".txt", sep="")),
                            header=FALSE, sep=" ", na.strings="NA", 
                            colClasses=c("integer"),
                            col.names = c("activity_id"))
    ds_activity_data
}

# get_features_data(subject_set, features_var_name)
# Read the feature data file containing the feature value of the subjects and return a datrafame
# with the values with the corresponding variable name provided by 
# the table feature_var_name
# Input : 
#   subject_set : name of the subject set (e.g. test, train)
#   features_var_name : list of feature file column name
# Return : data.frame
get_features_data <- function(subject_set, features_var_name) {
    ds_feature_data = fread(file.path(DATASET_DIR, subject_set, 
                            paste(FEATURES_FILE_PREFIX,subject_set,".txt", sep="")),
                            header=FALSE, sep=" ", na.strings="NA", 
                            col.names = features_var_name)
    ds_feature_data
}

# get_inertial_signal(subject_set, signal, device, axis)
# Note : this function is not used in the run_analysis script
# Read a specific the inertial data file containing the inertial signal of the subjects
# Input : 
#   subject_set : name of the subject set (e.g. test, train)
#   signal : signal type (e.g. body or total )
#   device : device (e.g. acc or gyro)
#   axis : spacial axis (e.g. x,y or z)
# Return : data.frame
get_inertial_signal <- function(subject_set, signal, device, axis, length=128) {
    
    name <- paste(signal, device, axis, "_", subject_set, sep="")
    signal_filename <- paste(name, ".txt", sep="")
    
    w_name <- paste(name, "_w", sep="")
    col_names = paste(w_name, seq(1:length), sep="")
    ds_inertial_signal = fread(file.path(DATASET_DIR, subject_set, 
                            INTERTIAL_SIGNALS_DIR, signal_filename),
                            header=FALSE, sep=" ", na.strings="NA",
                            col.names = col_names)
    ds_inertial_signal
}

# 0. Download file and unzip it
download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", "UCI_HAR_Dataset.zip", method="curl")
unzip(zipfile = "UCI_HAR_Dataset.zip")

# 1. Merges the training and the test sets to create one data set.
feature_var_name <- get_features_var_name()

# Read test set
subject_test <- get_subject_data(TEST_SET)
subject_test <- cbind( subject_test, 
                       get_activity_data(TEST_SET),
                       get_features_data(TEST_SET, feature_var_name$feature_var_name))
subject_test$set = TEST_SET

# Read train set
subject_train <- get_subject_data(TRAIN_SET)
subject_train <- cbind( subject_train,
                        get_activity_data(TRAIN_SET),
                        get_features_data(TRAIN_SET, feature_var_name$feature_var_name))
subject_train$set = TRAIN_SET

# Merge the two sets
subjects <- rbind(subject_test, subject_train)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# fitler column using the select from dplyr package
# based on the regulard expression : 
#  - subject_id
#  - set
#  - "-mean" for the mean
#  - "-std" for the standard deviation
subjects_std_mean <- select(subjects, matches("^subject_id$"), matches("^set$"), 
                        matches("^activity_id$"), contains("-mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- get_activity_labels()
subjects_std_mean <- merge(subjects_std_mean, activity_labels,all.x = TRUE, by = c("activity_id") )

# Reoder column and remove activity_id
columns_first <- c("subject_id", "set", "activity")
column_order <- c(columns_first, setdiff(names(subjects_std_mean), columns_first))
subjects_std_mean <- select(subjects_std_mean, one_of(column_order), -activity_id)

# 4. Appropriately labels the data set with descriptive variable names. 
# DONE IS STEP1
# This step was already done in the read function used in Step 1

# 5. From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject
subjects_tidy <- subjects_std_mean %>% 
                 group_by(subject_id, activity, set ) %>%
                 summarise_each(funs(mean))

# Write dataframe to csv file
write.table(subjects_tidy, file="subject_tidy.csv")