# Merges the training and the test sets to create one data set.

activityLabels <- read.table('./activity_labels.txt')

features <- read.table("./features.txt") #Take second column of features to be headers for xtrain and xtest

testX <- read.table("./test/X_test.txt")
subjectTest <- read.table("./test/subject_test.txt")
labelsTest <- read.table("./test/y_test.txt")

# inertiaSignalsTestTotal <- read.table('./test/Inertial Signals/total_acc_x_test.txt')
# inertiaSignalsTestBody <- read.table('./test/Inertial Signals/body_acc_x_test.txt')
# inertiaSignalsTestGyro <- read.table('./test/Inertial Signals/body_gyro_x_test.txt')

trainingX <- read.table("./train/X_train.txt")
subjectTraining <- read.table("./train/subject_train.txt")
labelsTrain <- read.table("./train/y_train.txt")
# inertiaSignalsTrainTotal <- read.table('./train/Inertial Signals/total_acc_x_train.txt')
# inertiaSignalsTrainBody <- read.table('./train/Inertial Signals/body_acc_x_train.txt')
# inertiaSignalsTrainGyro <- read.table('./train/Inertial Signals/body_gyro_x_train.txt')

#Okay now I need to put all this together
#Adding in labels
colnames(testX) <- features[,2]
colnames(trainingX) <- features[,2]
ytrain_with_labels <- left_join(labelsTrain,activityLabels,by = "V1")
ytest_with_labels <- left_join(labelsTest,activityLabels,by="V1")
#Renaming columns and adding in subject info
colnames(subjectTest)[colnames(subjectTest)=="V1"] <- "Subject"
colnames(subjectTraining)[colnames(subjectTraining)=="V1"] <- "Subject"
ytest_with_sub <- cbind(ytest_with_labels,subjectTest)
ytrain_with_sub <- cbind(ytrain_with_labels,subjectTraining)

#Putting it all together
training <- cbind(trainingX,ytrain_with_sub)
test <- cbind(testX,ytest_with_sub)
final <- rbind(test,training)

# Extracts only the measurements on the mean and standard deviation for each measurement.
#I'm interpreting this to mean I should take only the columns that have "std" or "mean" in the name

onlyMeanSTD <- cbind(final[,grepl("mean\\(",names(final))],final[,grepl("std",names(final))])

# Uses descriptive activity names to name the activities in the data set
#I did this earlier in the project - below is the code I used that you can reference above.
ytrain_with_labels <- left_join(labelsTrain,activityLabels,by = "V1")
ytest_with_labels <- left_join(labelsTest,activityLabels,by="V1")
colnames(final)[colnames(final)=="V2"] <- "Activity_Labels"

# Appropriately labels the data set with descriptive variable names.
colnames(testX) <- features[,2]
colnames(trainingX) <- features[,2]
#I also already did this above - see code I copied for reference :)

# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
onlyMeanSTD <- cbind(onlyMeanSTD,final$Subject,final$Activity_Labels)
colnames(onlyMeanSTD)[colnames(onlyMeanSTD)=="final$Subject"] <- "Subject"
colnames(onlyMeanSTD)[colnames(onlyMeanSTD)=="final$Activity_Labels"] <- "Activity_Labels"
tidy_data <- onlyMeanSTD %>% group_by(Activity_Labels,Subject) %>% summarise_all(mean)
write.table(tidy_data,file = "./run_analysis.txt",row.names = FALSE)

