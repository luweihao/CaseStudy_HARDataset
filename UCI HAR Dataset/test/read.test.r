## read data
load("SamsungData.test.rda")

    ########
SamsungData <- read.csv("X_test.csv", header = FALSE)

features <- read.csv("features.csv", header = FALSE)
colnames(SamsungData) <- t(features)

subject.test <- read.table("subject_test.txt")
SamsungData$subject <- subject.test
#> levels(as.factor(t(SamsungData_train$subject)))
#[1] "2"  "4"  "9"  "10" "12" "13" "18" "20" "24"

activity_labels.test <- read.table("y_test.txt")
SamsungData$activity_labels <-activity_labels.test

save(SamsungData, file = "SamsungData.test.rda")
