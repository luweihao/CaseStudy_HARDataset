###  Origin data       ######
SamsungData <- read.csv("X_test.csv", header = FALSE)
SamsungData_train <- read.csv("X_train.csv", header = FALSE)

###  Variables' names  ######  
features <- read.csv("features.csv", header = FALSE)
colnames(SamsungData) <- t(features)
colnames(SamsungData_train) <- t(features)


###  Subject index     ######
subject.test <- read.table("subject_test.txt")
SamsungData$subject <- subject.test
#> levels(as.factor(t(SamsungData_train$subject)))
#[1] "2"  "4"  "9"  "10" "12" "13" "18" "20" "24"

subject.train <- read.table("subject_train.txt")
SamsungData_train$subject <- subject.train
#> levels(as.factor(t(SamsungData_train$subject)))
#[1] "1"  "3"  "5"  "6"  "7"  "8"  "11" "14" "15" "16"
#[2] "17" "19" "21" "22" "23" "25" "26" "27" "28" "29" "30"


###  Activity labels   ######
activity_labels.test <- read.table("y_test.txt")
SamsungData$activity_labels <-activity_labels.test

activity_labels.train <- read.table("y_train.txt")
SamsungData_train$activity_labels <-activity_labels.train


###  Save data         ######
save(SamsungData, file = "SamsungData.test.rda")
save(SamsungData_train, file = "SamsungData.train.rda")

