#### read data 
load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = SamsungData_train[,1:561]
G_train=as.factor(t(SamsungData_train$activity_labels))
mydata = SamsungData[,1:561]
G=as.factor(t(SamsungData$activity_labels))
source("distinguish.distance.R")
source("distinguish.bayes.R")


#### distance method
distance.re=distinguish.distance(mydata_train, G_train, TstX=mydata, TstG=G)
#"system is exactly singular", means it fails to calculate the inverse matrix


#### bayes method
bayes.re=distinguish.bayes(mydata_train, G_train, TstX=mydata, TstG=G)
#"system is exactly singular", means it fails to calculate the inverse matrix
##So we try less variables
bayes.re=distinguish.bayes(mydata_train[,1:50], G_train, TstX=mydata[,1:50], TstG=G)
sum((bayes.re$origin-bayes.re$blong)==0)/nrow(mydata)  #accuracy rate


#### fisher method
mydatatrain=cbind(G_train, mydata_train)
mydatatest=cbind(G, mydata)

library(MASS)
ldist=lda(G_train~., data=mydatatrain)

pre.ldist=predict(ldist, newdata = mydatatest)
new.ind=pre.ldist$class

ori.group=mydatatest$G
tab1=table(ori.group, new.ind)
tab2=prop.table(tab1)
sum((as.numeric(ori.group)-as.numeric(new.ind))==0)/nrow(mydata)  #accuracy rate
print(tab1)
print(tab2)
## result with 96% accuracy, and it worth noticing that 
 # moving states(1,2,3) has been seperated from stationary states(4,5,6)
 # with higher efficient(only 1 fail)


#### preliminary classification
## get pre-group of test data
group=ceiling(as.numeric(new.ind)/3)
group_origin=ceiling(as.numeric(ori.group)/3)
mydata$group=group

## get pre-group of train data 
group2=ceiling(as.numeric(G_train)/3)
mydata_train$group=group2

## save the pre-group dataset
colnames(mydata_train)=c(1:561, "group")
G_train=SamsungData_train$activity_labels
mydata_train$G_train=G_train
colnames(mydata)=c(1:561, "group")
G=SamsungData$activity_labels
mydata$G=G
save(mydata_train, file = "mydata_train.rda")
save(mydata, file = "mydata.rda")
