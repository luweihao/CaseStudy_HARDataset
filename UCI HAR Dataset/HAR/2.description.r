###  Read data         ######
load("SamsungData.train.rda")
mydata_train = SamsungData_train[,1:561]
colnames(mydata_train)=1:561


###  Print names of variables  ######
names(SamsungData_train)[1:12]


###  Table number of each activity  ######
table(SamsungData_train$activity_labels)


###  Set sub1 for instance, describing the dataset  ######
sub1=SamsungData_train[SamsungData_train$subject==1,]
sub1=transform(sub1, activity_labels=factor(t(activity_labels)))
 #change activity_labels into factors


###  1st 2nd variables scatter plotting  ######
jpeg("describe1.jpeg", height=400, width = 600, quality = 100)
opar=par(mfrow=c(1,2))
plot(sub1[,1], col = sub1$activity_labels, ylab = names(sub1)[1])
plot(sub1[,2], col = sub1$activity_labels, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity_labels), col = unique(sub1$activity_labels), pch = 1)
dev.off()


###  11th 12th variables scatter plotting  ######
jpeg("describe2.jpeg", height=400, width = 600, quality = 100)
opar=par(mfrow=c(1,2))
plot(sub1[,10], pch = 19, col = sub1$activity_labels, ylab = names(sub1)[1])
plot(sub1[,11], pch = 19, col = sub1$activity_labels, ylab = names(sub1)[2])
legend("bottomright", legend = unique(sub1$activity_labels), col = unique(sub1$activity_labels), pch = 19)
dev.off()

 
###  Plotting of components analysis  ######
 #the following code in annotation may cost long time to run, and it will return
 #the suggested k, the number of principal components we advised to choose, and
 #plot the correltion between the heading 10 PCs and origin Xis
if(0){  #change to 1 to run
  library(psych)
  fa.parallel(mydata_train, fa = "pc", n.iter = 10, 
              show.legend = FALSE, main = "Scree plot with parallel analysis")
  #set parameter n.iter=10(it's advisable to have a larger n.iter)
  #"In factor.scores, the correlation matrix is singular, an approximation is used"
  #Parallel analysis suggests that the number of components =  44
  train.pr2=principal(mydata_train, nfactors=10, rotate="none")
  fa.diagram(train.pr2)
}