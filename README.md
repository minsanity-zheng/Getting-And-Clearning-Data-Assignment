# Getting And Cleaning Data Assignment
#-------------------------------------------------------------------------------
## 1. Merge the training and the test sets to create single data set.

### download zip file from website
if(!file.exists("./data")) dir.create("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, destfile = "./data/projectData_getCleanData.zip")

### unzip the downloaded data 
listZip <- unzip("./data/projectData_getCleanData.zip", exdir = "./data")

### load data unzipped data 
train.x <- read.table("./data/UCI HAR Dataset/train/X_train.txt")
train.y <- read.table("./data/UCI HAR Dataset/train/y_train.txt")
test.x <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
test.y <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
train.subject <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
test.subject <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
featureName <- read.table("./data/UCI HAR Dataset/features.txt", stringsAsFactors = FALSE)[,2]
activityName <- read.table("./data/UCI HAR Dataset/activity_labels.txt")

### merge the loaded train and test data.Through cbind, the subject columns are combined with train data columns and subject columns and also combines with test data columns test data 
trainData <- cbind(train.subject, train.y, train.x)
testData <- cbind(test.subject, test.y, test.x)
fullData <- rbind(trainData, testData)

#-------------------------------------------------------------------------------
## 2. Extract only the measurements on the mean and standard deviation for each measurement. 

featureIndex <- grep(("mean\\(\\)|std\\(\\)"), featureName)
finalData <- fullData[, c(1, 2, featureIndex+2)]
colnames(finalData) <- c("subject", "activity", featureName[featureIndex])

#-------------------------------------------------------------------------------
## 3. Uses descriptive activity names to name the activities in the data set

### replace 1 to 6 with activity names
finalData$activity <- factor(finalData$activity, levels = activityName[,1], labels = activityName[,2])

#-------------------------------------------------------------------------------
## 4. Appropriately labels the data set with descriptive variable names.
names(finalData) <- gsub("\\()", "", names(finalData))
names(finalData) <- gsub("^t", "Time", names(finalData))
names(finalData) <- gsub("^f", "Frequence", names(finalData))
names(finalData) <- gsub("-mean", "Mean", names(finalData))
names(finalData) <- gsub("-std", "Std", names(finalData))

#-------------------------------------------------------------------------------
## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### dplyr library is used for summarising the processed data
library(dplyr)
summarised_data <- finalData %>%
    group_by(subject, activity) %>%
    summarise_each(funs(mean))

### write the summarised data to file
write.table(summarised_data, file = "./MeanData.csv", append = FALSE, quote = TRUE, sep = ",",
            eol = "\n", na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"),
            fileEncoding = "")