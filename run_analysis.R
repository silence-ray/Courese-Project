library(dplyr)

#1.Download data from website and unzip data
if(!file.exists("./data")){dir.create("./data")}
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/projectData_getCleanData.zip", method = "curl")
unzip("./data/projectData_getCleanData.zip", exdir ="./data")

#2.read and name the data
#2.1 features & activity
features <- read.table("./data/UCI HAR Dataset/features.txt", col.names = c("n","functions"))
labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", col.names = c("code","activity"))
#2.2 train
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
train_x <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
train_y <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "code")
#2.3 test
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
test_x <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
test_y <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "code")

#3.Merges the training and the test sets to create one data set.
Train <- cbind(subject_train, train_y, train_x)
Test <- cbind(subject_test, test_y, test_x)
Merged_data <- rbind(Train, Test)

#4.Extracts only the measurements on the mean and standard deviation for each measurement.
MeanStd <- Merged_data %>% select(subject, code, contains("mean"), contains("std"))

#3.Uses descriptive activity names to name the activities in the data set
MeanStd$code <- labels[MeanStd$code, 2]

#4.Appropriately labels the data set with descriptive variable names.
names(MeanStd)[2] = "activity"
names(MeanStd)<-gsub("Acc", "Accelerometer", names(MeanStd))
names(MeanStd)<-gsub("Gyro", "Gyroscope", names(MeanStd))
names(MeanStd)<-gsub("BodyBody", "Body", names(MeanStd))
names(MeanStd)<-gsub("Mag", "Magnitude", names(MeanStd))
names(MeanStd)<-gsub("^t", "Time", names(MeanStd))
names(MeanStd)<-gsub("^f", "Frequency", names(MeanStd))
names(MeanStd)<-gsub("tBody", "TimeBody", names(MeanStd))
names(MeanStd)<-gsub("-mean()", "Mean", names(MeanStd), ignore.case = TRUE)
names(MeanStd)<-gsub("-std()", "STD", names(MeanStd), ignore.case = TRUE)
names(MeanStd)<-gsub("-freq()", "Frequency", names(MeanStd), ignore.case = TRUE)
names(MeanStd)<-gsub("angle", "Angle", names(MeanStd))
names(MeanStd)<-gsub("gravity", "Gravity", names(MeanStd))

#5.From the data set in step 4, creates a second, independent tidy data set with 
#the average of each variable for each activity and each subject.
FinalData <- MeanStd %>%
  group_by(subject, activity) %>%
  summarise_all(funs(mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)