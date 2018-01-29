## read data
load("SamsungData.train.rda")

        ########
SamsungData_train <- read.csv("X_train.csv", header = FALSE)

features <- read.csv("features.csv", header = FALSE)
colnames(SamsungData_train) <- t(features)

subject.train <- read.table("subject_train.txt")
SamsungData_train$subject <- subject.train
#> levels(as.factor(t(SamsungData_train$subject)))
#[1] "1"  "3"  "5"  "6"  "7"  "8"  "11" "14" "15" "16" "17" "19" "21" "22" "23" "25" "26" "27" "28" "29" "30"

activity_labels.train <- read.table("y_train.txt")
SamsungData_train$activity_labels <-activity_labels.train

save(SamsungData_train, file = "SamsungData.train.rda")
