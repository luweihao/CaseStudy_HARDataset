#### read data 
library(e1071)
load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = SamsungData_train[,1:561]
mydata_train$G_train=as.factor(t(SamsungData_train$activity_labels))
mydata = SamsungData[,1:561]
mydata$G=as.factor(t(SamsungData$activity_labels))

model1=svm(G_train~., data = mydata_train)
summary(model1)
#The SVM-Type item shows that the model category is C classifier model;
#The SVM-Kernel item shows that the kernel function used in this model 
#is a Gaussian inner product function and the value of the parameter gamma 
#in the kernel function is 0.001782531;
#The cost item shows that the cost of constraint violation determined by this model is 1;
#And we can also see that the model found 2330 support vectors:
#The first class contains 518 support vectors, the second 571, the third 234,
#the fourth 300, the fifth 358, and the sixth 349.
#The last line shows that the six categories in the model are 1, 2, 3, 4, 5, and 6, respectively.


pre=predict(model1, mydata[,-562])
sum((as.numeric(pre)-as.numeric(mydata$G))==0)/nrow(mydata)
table(mydata$G, pre)
