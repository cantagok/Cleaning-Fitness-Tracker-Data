library(data.table)
library(dplyr)

#Read Data
features <- read.table("./UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
activities <- read.table("./UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))

sub_test<-read.table("./UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
testx<-read.table("./UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
testy<-read.table("./UCI HAR Dataset/test/y_test.txt", col.names = "code")

sub_train<-read.table("./UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
trainx<-read.table("./UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
trainy<-read.table("./UCI HAR Dataset/train/y_train.txt", col.names= "code")

#Merges the training and the test sets to create one data set.
x<-rbind(trainx, testx)
y<-rbind(trainy, testy)
subject<-rbind(sub_train, sub_test)
merged<-cbind(subject, x, y)

#Extracts only the measurements on the mean 
#and standard deviation for each measurement.
tidy<-merged%>%select(subject, code, contains("mean"), contains("std"))

#Uses descriptive activity names to name the activities in the data set
tidy$code<-activities[tidy$code, 2]

#Appropriately labels the data set with descriptive variable names.
tidy<-tidy%>%rename(activity=code)
names(tidy)<-gsub("Acc", "Acclerometer", names(tidy))
names(tidy)<-gsub("Gyro", "Gyroscope",names(tidy))
names(tidy)<-gsub("BodyBody", "Body", names(tidy))
names(tidy)<-gsub("Mag", "Magnitude", names(tidy))
names(tidy)<-gsub("^t", "Time", names(tidy))
names(tidy)<-gsub("^f", "Frequency", names(tidy))
names(tidy)<-gsub("tBody", "TimeBody", names(tidy))
names(tidy)<-gsub("-mean()", "Mean", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-std()", "STD", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("-freq()", "Frequency", names(tidy), ignore.case = TRUE)
names(tidy)<-gsub("angle", "Angle", names(tidy))
names(tidy)<-gsub("gravity", "Gravity", names(tidy))

#From the data set in step 4, creates a second, independent tidy data 
#set with the average of each variable for each activity and each subject.
final <- tidy %>%
    group_by(subject, activity) %>%
    summarise_all(funs(mean))
write.table(final, "final.txt", row.name=FALSE)

