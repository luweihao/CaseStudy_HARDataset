#### read data
load("SamsungData.train.rda")
mydata_train = SamsungData_train[,1:561]
colnames(mydata_train)=1:561
G_train=SamsungData_train$activity_labels

mat_mydata_train=as.matrix(mydata_train)
mat_mydata_train.sd=scale(mat_mydata_train)


#### cluster analysis
km=kmeans(mat_mydata_train.sd, 6, nstart=100)
group=as.matrix(km$cluster)
group_origin=as.matrix(G_train)
## the index of the result, say, group, is diff from that of the origin
## so it seems hard to know it's accuracy rate
table(group_origin, group)


#### plot cluster result
library("cluster")
#km=kmeans(mat_mydata_train.sd, 6, nstart=100)
jpeg("kmeans.jpeg", height=1200, width = 1600, quality = 1000)
clusplot(mat_mydata_train.sd, km$cluster)
dev.off()
