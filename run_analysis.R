#Getting and Cleaning Data Course Projectless 

#The purpose of this project is to demonstrate your ability to collect,
#work with, and clean a data set. The goal is to prepare tidy data that can be
#used for later analysis. 

#1)Merges the training and the test sets to create one data set.
#2)Extracts only the measurements on the mean and standard deviation for each measurement.
#3)Uses descriptive activity names to name the activities in the data set
#4)Appropriately labels the data set with descriptive variable names.
#5)From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

################################

#load necessary packages

library(dplyr)
library(data.table)
library(tidyr)

#download the data files

#if(!file.exists("./samsungdata")){dir.create("./samsungdata")}
#fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
#download.file(fileUrl,destfile="./samsungdata/Dataset.zip",method="curl")

#Unzip the files 

#unzip(zipfile="./samsungdata/Dataset.zip",exdir="./data")
filespath <- file.path("./samsungdata" , "UCI HAR Dataset")

#Read the data from the data files into variables

#First, Read the Activity data

ActivityTest  <- read.table(file.path(filespath, "test" , "y_test.txt" ),header = FALSE)
ActivityTrain <- read.table(file.path(filespath, "train", "y_train.txt"),header = FALSE)

#Second, Read the Subject data 

SubjectTrain <- read.table(file.path(filespath, "train", "subject_train.txt"),header = FALSE)
SubjectTest  <- read.table(file.path(filespath, "test" , "subject_test.txt"),header = FALSE)

# Third, Read the Fearures data

FeaturesTest  <- read.table(file.path(filespath, "test" , "X_test.txt" ),header = FALSE)
FeaturesTrain <- read.table(file.path(filespath, "train", "X_train.txt"),header = FALSE)

# Step1 : Merges the training and the test sets to create one data set

#merge the rows of subject data and set name
dataSubject <- rbind(SubjectTrain, SubjectTest)
setnames(dataSubject, "V1", "subject")

#merge the rows of activity data and set the name
dataActivity<- rbind(ActivityTrain, ActivityTest)
setnames(dataActivity, "V1", "activity")

#merge the rows of the features data and set the name

dataFeatures<- rbind(FeaturesTrain, FeaturesTest)
dataFeaturesNames <- read.table(file.path(filespath, "features.txt"),header =FALSE)
names(dataFeatures)<- dataFeaturesNames$V2

#merge the columns and finished the data merging

datasubandact <- cbind(dataSubject, dataActivity)
FinalDataset <- cbind(dataFeatures, datasubandact)

#Step 2:  Extracts only the measurements on the mean and standard 
#deviation for each measurement.

data_mean_std <- dataFeaturesNames$V2[grep("mean\\(\\)|std\\(\\)", dataFeaturesNames$V2)]
selectedNames<-c(as.character(data_mean_std), "subject", "activity" )
FinalDataset < -subset(FinalDataset,selectdebug=selectedNames)

#Step 3: Uses descriptive activity names to name the activities in the data set

#read the activity lables 

activityLabels<- read.table(file.path(filespath, "activity_labels.txt"), header = FALSE)
setnames(activityLabels, names(activityLabels), c("activity","activityName"))
FinalDataset <- merge(activityLabels, FinalDataset , by="activity", all.x=TRUE)
FinalDataset$activityName <- as.character(FinalDataset$activityName)


#Step4:Appropriately labels the data set with descriptive variable names

names(FinalDataset)<-gsub("std()", "SD", names(FinalDataset))
names(FinalDataset)<-gsub("mean()", "MEAN", names(FinalDataset))
names(FinalDataset)<-gsub("^t", "time", names(FinalDataset))
names(FinalDataset)<-gsub("^f", "frequency", names(FinalDataset))
names(FinalDataset)<-gsub("Acc", "Accelerometer", names(FinalDataset))
names(FinalDataset)<-gsub("Gyro", "Gyroscope", names(FinalDataset))
names(FinalDataset)<-gsub("Mag", "Magnitude", names(FinalDataset))
names(FinalDataset)<-gsub("BodyBody", "Body", names(FinalDataset))

#From the data set in step 4, creates a second, independent tidy data set 
#with the average of each variable for each activity and each subject.

tidyDatasecond<-aggregate(.~subject + activity, FinalDataset, mean)
tidyDatasecond<-tidyDatasecond[order(tidyDatasecond$subject,tidyDatasecond$activity),]
write.table(tidyDatasecond, file = "tidydata.txt",row.name=FALSE)


