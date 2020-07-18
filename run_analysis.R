
# Getting and Cleaning Data Course Project - Coursera

## Developed by Leonardo Moreno

### The goal of this project is create a tidy data set from Human Activity Recognition Using Smartphones Data Set
### This task was develop for Getting and Cleaning Data Course

### 1. Load the data 

library(dplyr)

#### Column Labels (Variables)

features<-read.table("UCI HAR Dataset/features.txt",header=FALSE)
features<-as.character(features[,2])
activity_labels<-read.table("UCI HAR Dataset/activity_labels.txt",header = FALSE)
activity_labels<-as.character(activity_labels[,2])



#### Train Data

train_data<-read.table("UCI HAR Dataset/train/X_train.txt",header =FALSE) ## Train Raw Data
train_labels<-read.table("UCI HAR Dataset/train/y_train.txt",header =FALSE) ## Activity Index

subject_train<-read.table("UCI HAR Dataset/train/subject_train.txt") ## ID Train index = SUbjects Train

train_group<-rep(1,7352) # 1= Train group in Final Dataset

DB_Train<-cbind(subject_train,train_group,train_labels,train_data) # Merge Train Data


names(DB_Train)<-c(c("ID","Group","activity"), features) # Column Names



str(DB_Train)

#### Test Data
test_data<-read.table("UCI HAR Dataset/test/X_test.txt",header =FALSE) ## Test raw data
test_labels<-read.table("UCI HAR Dataset/test/y_test.txt",header =FALSE) ## Activity Index

subject_test<-read.table("UCI HAR Dataset/test/subject_test.txt") ## ID test Index=Subjects test

test_group<- rep(2,2947) # 2= Test group in Final Dataset

DB_Test<- cbind(subject_test,test_group,test_labels,test_data) #Merge Test data

names(DB_Test)<-c(c("ID","Group","activity"), features) #Column Names

str(DB_Test)

#### 2.Unify Databases (Merge)

DB_Total <-rbind(DB_Train,DB_Test)

DB_Total$Group<-factor(DB_Total$Group,levels=c(1,2),
                       labels=c("Train","Test")) #Change Group labels

DB_Total$activity<-factor(DB_Total$activity,levels=1:6,
                          labels = activity_labels) #Change activity labels

str(DB_Total)

#### 3. Select Variables

DB_Final<- DB_Total[,which(colnames(DB_Total) %in% c("ID", "Group",
         "activity", grep(("mean()|std()"), colnames(DB_Total), value=TRUE)))] #Select mean() and sd()measures
str(DB_Final)
View(DB_Final)

DB_Final2<-select(DB_Final,- which(colnames(DB_Final) %in%
                grep(("Freq"), colnames(DB_Final), value=TRUE))) #Depurate Columns with "Freq"

View(DB_Final2)

#### 4. Rename columns with descriptive labels

## I use the full name of each abbreviation (see the codebook for reference) 
##include "_" as separator
## replace the x,y and z index for (x),(y) and (z) inside the function label (e.g. mean(), std())

### Replace abbreviations and include separators (Note: Run just one time!)

names(DB_Final)[-c(1:3)]<-gsub("^t", "time_", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub("^f", "frequency_", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub("Acc", "_Accelerometer", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub("Gyro", "_Gyroscope", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub("Mag", "_Magnitude", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub("BodyBody", "Body_", names(DB_Final)[-c(1:3)])

### Replace functions labels
ch_y<-as.character("[(][)]-Y$")
ch_z<-as.character("[(][)]-Z$")
ch_x<-as.character("[(][)]-X$")

names(DB_Final)[-c(1:3)]<-gsub(ch_y, "(Y)", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub(ch_z, "(Z)", names(DB_Final)[-c(1:3)])
names(DB_Final)[-c(1:3)]<-gsub(ch_x, "(X)", names(DB_Final)[-c(1:3)])

str(DB_Final)
View(DB_Final)

#### 5. Tidy summary tables
#### Independient table with mean summary per subject,group and activity

## First option
DB_summary<-aggregate(.~ID+Group+activity,DB_Final,mean)
DB_summary<-DB_summary[order(DB_summary$ID,DB_summary$Group,DB_summary$activity),]

str(DB_summary)
View(DB_summary)

## Second option

DB_s<-DB_Final %>% group_by(ID,Group,activity) %>% summarise_all(list(mean))
DB_s<-DB_s[order(DB_s$ID,DB_s$Group,DB_s$activity),]

str(DB_s)
View(DB_s)

## The end, Thanks for Review Me!
##NOTE:Check the codebook for the variable codes and additional info

write.csv(DB_Final,"DB_UCI_HAR_Tidy.csv",row.names=FALSE)
write.csv(DB_s,"Summary_UCI_HAR_Tidy.csv",row.names=FALSE)

write.table(DB_Final,"DB_UCI_HAR_Tidy.txt",row.names=FALSE)
write.table(DB_s,"Summary_UCI_HAR_Tidy.txt",row.names=FALSE)