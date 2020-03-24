# CourseProject-GettingandCleaningData

#-------------------------------Folder UCI HAR-----------------------------------
# Reading data

activity_labels <- readLines("./rawData/UCI HAR Dataset/activity_labels.txt")
features <- readLines("./rawData/UCI HAR Dataset/features.txt")
subject_test <- read.table("./rawData/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("rawData/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("rawData/UCI HAR Dataset/test/y_test.txt")
x_train <- read.table("./rawData/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./rawData/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./rawData/UCI HAR Dataset/train/subject_train.txt")



#---------------- Preparing Data -------------------------------------------------

# Separating the initial number from each features name
splitNames <- strsplit(features," ") 
# Removing the initial number from each feature name
features <- sapply(splitNames, function(x){x[2]}) 

# Naming each column in x_test according to the labels in features
names(x_test) <- features 
names(y_test) <- "ActivityLabels" # Renaming the variable 
names(subject_test) <- "Subject" 

names(x_train) <- features
names(y_train) <- "ActivityLabels"
names(subject_train) <- "Subject"

#-----------------------------Merging datasets---------------------------------

test_tb <- cbind(subject_test, y_test, x_test)
train_tb <- cbind(subject_train, y_train, x_train)

# using rbind to join test_tb and train_tb
har_tb <- rbind(test_tb, train_tb)
har_tb <- har_tb[order(har_tb$Subject, har_tb$ActivityLabels),]
backup <- har_tb

col2 <- gsub("1","WALKING", har_tb$ActivityLabels)
col2 <- gsub("2","WALKING_UPSTAIRS", col2)
col2 <- gsub("3","WALKING_DOWNSTAIRS", col2)
col2 <- gsub("4","SITTING", col2)
col2 <- gsub("5","STANDING", col2)
col2 <- gsub("6","LAYING", col2)
har_tb$ActivityLabels <- col2

#---------Extracts only the measurements on the mean and standard deviation for each measurement-----

#Extracting all names from har_tb
my_var <- names(har_tb) 
#Subsetting only names with "mean", and "std"
pos_a <- grep("mean",my_var);pos_b <- grep("std",my_var) 
#adding columns 1 e 2, subject and Activity_labels
pos_c <- c(1,2) 
#combining all variables 
pos_var <- c(pos_a,pos_b,pos_c) 
#81 variables that will be subsetted from hr_tb
length(unique(pos_var)) 
#sorting pos_var
pos_var <- sort(pos_var) 
#Subsetting hr_tb
har_tb_sub <- har_tb[,pos_var] 

  
#--------------- Taking the mean of all duplicated Subject and ActivityLabels----------------------

library(dplyr)
har <- har_tb_sub %>%
group_by(Subject, ActivityLabels) %>%
summarise_all(funs(mean))

#Finishing activity by creating a tidy dataset
write.table(har, file = "./tidyData.txt", row.names = FALSE)

