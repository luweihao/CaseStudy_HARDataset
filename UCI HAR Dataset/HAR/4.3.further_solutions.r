#### dicriminant analysis #######
#### read data 
load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = SamsungData_train[,1:561]
G_train=as.factor(t(SamsungData_train$activity_labels))
mydata = SamsungData[,1:561]
G=as.factor(t(SamsungData$activity_labels))
source("distinguish.distance.R")
source("distinguish.bayes.R")


#### fisher method
mydatatrain=cbind(G_train, mydata_train)
mydatatest=cbind(G, mydata)

library(MASS)
ldist=lda(G_train~., data=mydatatrain)

pre.ldist=predict(ldist, newdata = mydatatest)
new.ind=pre.ldist$class

## discriminant results
ori.group=mydatatest$G
tab1=table(ori.group, new.ind)
print(tab1)


#### detailed classification
## get pre-group of test data
group=rep(6, length(new.ind))
group[new.ind==3]=3
group[new.ind==1]=1
group[new.ind==2]=1
group[new.ind==4]=2
group[new.ind==5]=2
mydata$group=group

## get pre-group of train data
G_train=SamsungData_train$activity_labels
group2=rep(6, length(G_train))
group2[G_train==3]=3
group2[G_train==1]=1
group2[G_train==2]=1
group2[G_train==4]=2
group2[G_train==5]=2
mydata_train$group=group2

## save the pre-group dataset
colnames(mydata_train)=c(1:561, "group")
mydata_train$G_train=G_train
colnames(mydata)=c(1:561, "group")
G=SamsungData$activity_labels
mydata$G=G


### search
source("pcafunction.r")
acc1=(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=1, nnc=seq(20,300,10)))$acc
acc2=(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=2, nnc=seq(20,300,10), fixed=TRUE))$acc

## acc: the wrong rate
acc=as.data.frame(cbind(t(acc1), t(acc2)[,2]))
colnames(acc)=c("k", "G1", "G2")


#### plot G-k graph
jpeg("bestk.d.jpeg", height=500, width = 600, quality = 100)
with(acc, {
  plot(k, G1, pch = 20, col = "red", ylab = "G1 & G2", ylim = c(0.03,0.15));
  lines(lowess(k, G1), col = "red", lwd =2, lty = 1);
  points(k, G2, pch = 19, col = "blue");
  lines(lowess(k, G2), col = "blue", lwd =2, lty = 1);
  legend(250, 0.10, pch = c(20, 19), col = c("red", "blue"), legend = c("G1", "G2"))
})
dev.off()


#### write the result
write.csv(acc,file="acc.d.csv")


##in detailed od best k
print(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=1, nnc=210, printtable = TRUE))
print(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=2, nnc=230, printtable = TRUE, fixed=TRUE))
