run_analysis <- function(directory = "UCIHARDataset") {
# 1/ Merges the training and the test sets to create one data set.
#directory = "UCIHARDataset"
	setwd("~/Coursera/data/")
	setwd(directory)

	Features <- read.table("features.txt",sep="", dec=".",header = FALSE) ## 'features.txt': List of all features.

	Activity_labels <- read.table("activity_labels.txt",sep="", dec=".",header = FALSE) ## 'activity_labels.txt': Links the class labels with their activity name.

	names(Activity_labels) <- c("class labels", "activity name")

	setwd("train") 
	Train_y_text <- read.table("y_train.txt", 			sep="", dec=".",header = FALSE) ## 'train/y_train.txt': Training labels.
	Train_X_text <- read.table("X_train.txt",			sep="", dec=".",header = FALSE) ## 'train/X_train.txt': Training set.
	Train_Subject <- read.table("subject_train.txt",	sep="", dec=".",header = FALSE) ## 'subject_train.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30. 

	Train_files <- list.files("Inertial Signals")
	
	setwd("../")

	setwd("test")
	Test_y_text <- read.table("y_test.txt", 		sep="", dec=".",header = FALSE) ## 'test/y_test.txt': Test labels.
	Test_X_text <- read.table("X_test.txt", 		sep="", dec=".",header = FALSE) ## 'test/X_test.txt': Test set.
	Test_Subject <- read.table("subject_test.txt",	sep="", dec=".",header = FALSE) ## 'subject_test.txt': Each row identifies the subject who performed the activity for each window sample. Its range is from 1 to 30.

	Test_files <- list.files("Inertial Signals")
	
	setwd("~/Coursera/data/")
	
	TestData <- cbind(Test_Subject, Test_y_text, Test_X_text)
	TrainData <- cbind(Train_Subject, Train_y_text, Train_X_text)
	MergedData <- rbind(TestData, TrainData)


# 	2/ Extracts only the measurements on the mean and standard deviation for each measurement.  
	MeanIndices <- grep("mean\\(\\)",Features$V2) ## Extract column indices with character string == "mean()"
	MeanIndices <- MeanIndices + 2 ## offset by 2 to match the subjet and activity names
	STDIndices <- grep("std\\(\\)",Features$V2) ## Extract column indices with character string == "std()"
	STDIndices <- STDIndices + 2 ## offset by 2 to match the subjet and activity names
	MergedData_MeanSTD <- cbind(MergedData[,1:2], MergedData[,MeanIndices], MergedData[,STDIndices])
	
	
# 	3/ Uses descriptive activity names to name the activities in the data set
	MergedData_MeanSTD[,2] <- Activity_labels[ MergedData_MeanSTD[,2], 2]

# 	4/ Appropriately labels the data set with descriptive variable names. 

	MergedData_ColNames <- c("Subject", "Activity", as.vector(Features$V2[MeanIndices]), as.vector(Features$V2[MeanIndices]))
	
	names(MergedData_MeanSTD) <- MergedData_ColNames
	write.table(MergedData_MeanSTD, "MergedData_MeanSTD.txt", row.name=FALSE)


# 	5/ Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
	tidydataset <- data.frame()
	tidydataset <- data.frame(t(rep(NA,68)))
	rownumber <- 1
	for (i in 1:30) {
		subjet <- (MergedData_MeanSTD["Subject"] == i)
		for (j in Activity_labels[,2]) {
			tidydataset[rownumber,1] = i
			tidydataset[rownumber,2] = j
			activity <- (MergedData_MeanSTD["Activity"] == j)
			tidydataset[rownumber,3:68] <- colMeans(MergedData_MeanSTD[subjet & activity, 3:68])
			rownumber <- rownumber + 1
		}
	}
	names(tidydataset) <- names(MergedData_MeanSTD)
	tidydataset <- tidydataset[-1,] ## remove row 1 
	write.table(tidydataset, "tidydataset.txt", row.name=FALSE)

	return(tidydataset)
}
