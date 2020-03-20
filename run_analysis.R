activity_labels <- readLines("./rawData/UCI HAR Dataset/activity_labels.txt")
features <- readLines("./rawData/UCI HAR Dataset/features.txt")
subject_test <- read.table("./rawData/UCI HAR Dataset/test/subject_test.txt")
x_test <- read.table("rawData/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("rawData/UCI HAR Dataset/test/y_test.txt")
x_train <- read.table("./rawData/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./rawData/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("./rawData/UCI HAR Dataset/train/subject_train.txt")


splitNames <- strsplit(features," ") 
features <- sapply(splitNames, function(x){x[2]})

names(x_test) <- features
names(y_test) <- "ActivityLabels"
names(subject_test) <- "Subject" 
names(x_train) <- features
names(y_train) <- "ActivityLabels"
names(subject_train) <- "Subject"

test_tb <- cbind(subject_test, y_test, x_test)
train_tb <- cbind(subject_train, y_train, x_train)

har_tb <- rbind(test_tb, train_tb)
har_tb <- har_tb[order(har_tb$Subject, har_tb$ActivityLabels),]

col2 <- gsub("1","WALKING", har_tb$ActivityLabels)
col2 <- gsub("2","WALKING_UPSTAIRS", col2)
col2 <- gsub("3","WALKING_DOWNSTAIRS", col2)
col2 <- gsub("4","SITTING", col2)
col2 <- gsub("5","STANDING", col2)
col2 <- gsub("6","LAYING", col2)
har_tb$ActivityLabels <- col2

my_var <- names(har_tb)
pos_a <- grep("mean",my_var);pos_b <- grep("std",my_var)
pos_c <- c(1,2)
pos_var <- c(pos_a,pos_b,pos_c)
length(unique(pos_var))
pos_var <- sort(pos_var)
har_tb_sub <- har_tb[,pos_var] 

library(dplyr)
har <- har_tb_sub %>%
group_by(Subject, ActivityLabels) %>%
summarise_all(funs(mean))

write.table(har, file = "./tidyData.txt", row.names = FALSE)