## Task: create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for 
#    each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.

# In this project, the analyst delivers:
# run_analysis.R : the R-code run on the data set
# Tidy.txt : the clean data extracted from the original data using run_analysis.R
# CodeBook.md : the CodeBook reference to the variables in Tidy.txt
# README.md : the analysis of the code in run_analysis.R
# analysis.html : the html version of README.md

# Required to submit: 
# 1. a tidy data set as described below, 
# 2. a link to a Github repository with your script for performing the analysis, and 
# 3. a code book that describes the variables, the data, and any transformations 
# or work that you performed to clean up the data called CodeBook.md. 
# You should also include a README.md in the repo with your scripts. This repo 
# explains how all of the scripts work and how they are connected.

library(dplyr)
library(tidyr)
library(data.table)

# Get the data into R

### Read in the metadata for descriptive names
labelsActivity <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE)
namesFeatures  <- read.table("UCI HAR Dataset/features.txt")

### Read Testing data from the uncompressed subdirectory in the project directory
TestingSubject  <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
TestingFeatures <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
TestingActivity <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)

### Read Training data from the uncompressed subdirectory in the project directory
TrainingSubject  <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)
TrainingFeatures <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
TrainingActivity <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)

# Part 1: Merges the training and the test sets to create one data set.
activity <- rbind(TestingActivity, TrainingActivity)
features <- rbind(TestingFeatures, TrainingFeatures)
subject  <- rbind(TestingSubject, TrainingSubject)

### Descriptive feature names are added to the data columns after tranposing the column to a row vector
colnames(features) <- t(namesFeatures$V2)

### Make the final data product for analysis
colnames(activity) <- "Activity"
colnames(subject)  <- "Subject"
mergedDataset      <- cbind(subject, activity, features)


# Part 2: Extracts only the measurements on the mean and standard deviation for each measurement.
statistics <- grep(".*Mean.*|.*Std.*", names(mergedDataset), ignore.case=TRUE)

### Identify columns with mean and standard diviation stastics
statisticalColumns <- c(1, 2, statistics)


### Subset the merged data set to only the statistics data set
statisticalData <- mergedDataset[,statisticalColumns]


# Part 3: Uses descriptive activity names to name the activities in the data set
statisticalData$Activity <- as.character(statisticalData$Activity)  # convert to character

### Iterate through activity labels, subsitute the activity description from column 2 into the data
description <- 2  # second column has the description

for(row in seq_along(labelsActivity$V1)){  
        statisticalData$Activity[statisticalData$Activity == row] <- as.character(labelsActivity[row,description])  
}  
statisticalData$Activity <- as.factor(statisticalData$Activity)  # change to factor


# Part 4. Appropriately labels the data set with descriptive variable names. 

### Produce the abbreviated variable names
names(statisticalData)

### Unabbreviate all variable names :
names(statisticalData) <- gsub("Acc", "Accelerometer", names(statisticalData))  
names(statisticalData) <- gsub("Gyro", "Gyroscope", names(statisticalData))  
names(statisticalData) <- gsub("BodyBody", "Body", names(statisticalData))  
names(statisticalData) <- gsub("Mag", "Magnitude", names(statisticalData))  
names(statisticalData) <- gsub("^t", "Time", names(statisticalData))  
names(statisticalData) <- gsub("^f", "Frequency", names(statisticalData))  
names(statisticalData) <- gsub("tBody", "TimeBody", names(statisticalData))  
names(statisticalData) <- gsub("-mean()", "Mean", names(statisticalData), ignore.case = TRUE)  
names(statisticalData)   <- gsub("-std()", "StandardDeviation", names(statisticalData), ignore.case = TRUE)
names(statisticalData) <- gsub("-freq()", "Frequency", names(statisticalData), ignore.case = TRUE)  
names(statisticalData) <- gsub("angle", "Angle", names(statisticalData))  
names(statisticalData) <- gsub("gravity", "Gravity", names(statisticalData))  
names(statisticalData) <- gsub("X", "X-Coordinate", names(statisticalData))  
names(statisticalData) <- gsub("Y", "Y-Coordinate", names(statisticalData))  
names(statisticalData) <- gsub("Z", "Z-Coordinate", names(statisticalData))  


# Produce the final unabbreviated variable names 
names(statisticalData)


# Part 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
answer <- data.table()
answer <- statisticalData %>% group_by(Subject, Activity) %>% summarise_each(funs(mean))
answer <- answer[order(answer$Subject, answer$Activity),]
write.table(answer, file = "Tidy.txt", row.names = FALSE)  



