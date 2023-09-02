
##################################################################3
# Load packages 

library(readr)
library(tidyverse)

################################################################################

### Load Train set and merge  

train_set<-read_table2("UCI HAR Dataset/train/X_train.txt", 
                          col_names = FALSE)

train_labels <- read_csv("UCI HAR Dataset/train/y_train.txt", 
                         col_names = 'activitie')

subject_train <- read_csv("UCI HAR Dataset/train/subject_train.txt", 
                         col_names = "subject")

train_set_all<-cbind(subject_train,train_labels,train_set)



###################################################################
### Load test set and merge
test_set <- read_table2("UCI HAR Dataset/test/X_test.txt", 
                        col_names = FALSE)

test_labels <- read_csv("UCI HAR Dataset/test/y_test.txt", 
                        col_names = "activitie")

subject_test <- read_csv("UCI HAR Dataset/test/subject_test.txt", 
                         col_names = "subject")

test_set_all<-cbind(subject_test,test_labels,test_set)

#################################################################

### 1. Merges the training and the test sets to create one data set.

data_set<-rbind(train_set_all,test_set_all)

###################################################################

### 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

### sum(colSums(is.na(data_set)))

Means<-colMeans(data_set[,c(-1,-2)])
StandarDeviarion<- apply(data_set[,c(-1,-2)], 2, sd)

#######################################################################

###  3 Uses descriptive activity names to name the activities in the data set


activity_labels <- read_table2("UCI HAR Dataset/activity_labels.txt", 
                               col_names = c('activitie','name_act'))


data_set<-data_set %>% left_join(activity_labels,by='activitie')

####################################################################

### 4. Appropriately labels the data set with descriptive variable names. 


data_set <- data_set %>% dplyr::select(subject,activitie,name_act,everything())


variables_names<- read_table2("UCI HAR Dataset/features.txt", 
                              col_names = FALSE)

variables_names<- variables_names$X2

new_names <- gsub("(\\b\\w+\\b)(\\s+)(\\w+)", "\\1\\2\\U\\3",variables_names, perl = TRUE)


variables_names<-paste(seq(1:561),new_names,sep = "")

names(data_set)<-c("subject","activitie","name_act",variables_names)

#################################################################

### independent tidy data set with the average of each variable for each activity and each subject.

data_set_mean_subject<- data_set %>% 
  group_by(activitie,subject) %>%
  summarise_at(variables_names,mean)
  
  
  
  
  


