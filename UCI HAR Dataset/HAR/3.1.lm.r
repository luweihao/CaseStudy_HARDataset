#### read data 
load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = as.data.frame(SamsungData_train[,-562])
mydata_train$activity_labels=as.numeric(mydata_train$activity_labels[[1]])
colnames(mydata_train)=c(paste("x",1:561,sep=""), "G_train")

mydata = as.data.frame(SamsungData[,-562])
mydata$activity_labels=as.numeric(mydata$activity_labels[[1]])
colnames(mydata)=c(paste("x",1:561,sep=""), "G")
G=mydata$G
new=mydata[,-562]


#### lm model
lm.train=lm(G_train~., data = mydata_train)
G_pre=round(predict(lm.train, new))
G_pre[G_pre<=0]=1
G_pre[G_pre>=7]=6


#### conclusions
sum((G-G_pre)!=0)/nrow(mydata) #error rate
table(as.matrix(G), G_pre)


#### condition number
XX=cor(mydata_train[,-562])
kappa(XX)

