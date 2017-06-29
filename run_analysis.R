# 1. Merge the training and the test sets to create one data set.
# Load the data from files
features     = read.table('./features.txt',header=FALSE)
activityType = read.table('./activity_labels.txt',header=FALSE) 
subjectTrain = read.table('./train/subject_train.txt',header=FALSE)
xTrain       = read.table('./train/x_train.txt',header=FALSE)
yTrain       = read.table('./train/y_train.txt',header=FALSE)

# Assigin column names to the data
colnames(activityType)  = c('activityId','activityType')
colnames(subjectTrain)  = "subjectId"
colnames(xTrain)        = features[,2] 
colnames(yTrain)        = "activityId"

trainingData = cbind(yTrain,subjectTrain,xTrain)

# Load the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE) 
xTest       = read.table('./test/x_test.txt',header=FALSE) 
yTest       = read.table('./test/y_test.txt',header=FALSE) 

colnames(subjectTest) = "subjectId"
colnames(xTest)       = features[,2]
colnames(yTest)       = "activityId"
testData = cbind(yTest,subjectTest,xTest)

# Combine training and test data to create a final data set
AllData = rbind(trainingData,testData)

# 2. Extract only the measurements on the mean and standard deviation for each measurement. 
colNames <- colnames(AllData)
meanAndstd <- (grepl("activityId" , colNames) | 
                 grepl("subjectId" , colNames) | 
                 grepl("mean.." , colNames) | 
                 grepl("std.." , colNames) 
)
finalData <- AllData[ , meanAndstd== TRUE]

# 3. Use descriptive activity names to name the activities in the data set
finalData = merge(finalData,activityType,by='activityId',all.x=TRUE)

# 4. Appropriately label the data set with descriptive activity names. 
names(finalData) = gsub("\\()","",names(finalData))
names(finalData)= gsub("^(t)","time",names(finalData))
names(finalData)= gsub("^(f)","freq",names(finalData))
names(finalData)=gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",names(finalData))
names(finalData) = gsub("Mag","Magnitude",names(finalData))
names(finalData) <-gsub("Acc", "Accelerometer", names(finalData))

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType']

# Include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId,subjectId = finalDataNoActivityType$subjectId),mean)

# Include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE)

# Create the tidyData set 
write.table(tidyData, './tidyData.txt',row.names=TRUE,sep='\t')

