# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected. 
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# Here are the data for the project:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
# You should create one R script called run_analysis.R that does the following. 

## 'features.txt': List of all features.
## 'activity_labels.txt': Links the class labels with their activity name.
## 'train/X_train.txt': Training set.
## 'train/y_train.txt': Training labels.
## 'test/X_test.txt': Test set.
## 'test/y_test.txt': Test labels.

# The following files are available for the train and test data. Their descriptions are equivalent. 
# - 'train/subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 
# - 'train/Inertial Signals/total_acc_x_train.txt': The acceleration signal from the smartphone accelerometer X axis in standard gravity units 'g'. Every row shows a 128 element vector. The same description applies for the 'total_acc_x_train.txt' and 'total_acc_z_train.txt' files for the Y and Z axis. 
# - 'train/Inertial Signals/body_acc_x_train.txt': The body acceleration signal obtained by subtracting the gravity from the total acceleration. 
# - 'train/Inertial Signals/body_gyro_x_train.txt': The angular velocity vector measured by the gyroscope for each window sample. The units are radians/second. 

run_analysis <- function(directory = "UCIHARDataset") {
	
	setwd("~/Coursera/data/")
	setwd(directory)
	
	Features <- read.table("features.txt",sep="", dec=".",header = FALSE)	
	Activity_labels <- read.table("activity_labels.txt",sep="", dec=".",header = FALSE)
	names(Activity_labels) <- c("class labels", "activity name")

	setwd("train")
	Train_y_text <- read.table("y_train.txt", 			sep="", dec=".",header = FALSE)
	Train_X_text <- read.table("X_train.txt",			sep="", dec=".",header = FALSE)
	Train_Subject <- read.table("subject_train.txt",	sep="", dec=".",header = FALSE)
	
	Train_files <- list.files("Inertial Signals")
	
	
	setwd("../")

	setwd("test")
	Test_y_text <- read.table("y_test.txt", 		sep="", dec=".",header = FALSE)
	Test_X_text <- read.table("X_test.txt", 		sep="", dec=".",header = FALSE)
	Test_Subject <- read.table("subject_test.txt",	sep="", dec=".",header = FALSE)

	Test_files <- list.files("Inertial Signals")
	
	setwd("~/Coursera/data/")
	
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names. 
	names(Test_X_text) <- as.vector(Features$V2)
	names(Test_Subject) <- "subject id"
	names(Test_y_text) <- "activity name"
	Test <- cbind(Test_Subject, Test_y_text, Test_X_text)
	
	names(Train_X_text) <- as.vector(Features$V2)
	names(Train_Subject) <- "subject id"
	names(Train_y_text) <- "activity name"
	Train <- cbind(Train_Subject, Train_y_text, Train_X_text)

## Merges the training and the test sets to create one data set.
	MergedData_Test_Train <- rbind(Test, Train)

	write.table(MergedData_Test_Train, "MergedData_Test_Train.txt", row.name=FALSE)

# Extracts only the measurements on the mean and standard deviation for each measurement. 
	MergedData_Test_Train_Mean <- colMeans(MergedData_Test_Train[,3:563])
	MergedData_Test_Train_SD <- apply(MergedData_Test_Train[,3:563],2,sd)

# Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
	tidydataset <- data.frame()
	tidydataset <- data.frame(t(rep(NA,563)))
	names(tidydataset) <- names(MergedData_Test_Train)
		
	for (i in 1:30) {
		subjet <- (MergedData_Test_Train["subject id"] == i)
		for (j in 1:5) {
			activity <- (MergedData_Test_Train["activity name"] == j)
			tidydataset <- rbind(tidydataset, colMeans(MergedData_Test_Train[subjet & activity, ]))
		}
	}
	tidydataset <- tidydataset[-1,] ## remove row 1 
	return(tidydataset)
}
