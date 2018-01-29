load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = SamsungData_train[,-562]
colnames(mydata_train)=c(1:561, "G_train")
mydata = SamsungData[,-562]
colnames(mydata)=c(1:561, "G")
mrange<-matrix(0,36,561)
for(i in 1:561){
  for(j in 1:6){
    mrange[6*j-5,i]=quantile(subset(mydata_train[,i], G_train==j), 0.025)
    mrange[6*j-4,i]=quantile(subset(mydata_train[,i], G_train==j), 0.100)
    mrange[6*j-3,i]=quantile(subset(mydata_train[,i], G_train==j), 0.250)
    mrange[6*j-2,i]=quantile(subset(mydata_train[,i], G_train==j), 0.750)
    mrange[6*j-1,i]=quantile(subset(mydata_train[,i], G_train==j), 0.900)
    mrange[6*j  ,i]=quantile(subset(mydata_train[,i], G_train==j), 0.975)
  }
}


G_pre=rep(0, nrow(mydata))
for(m in 1:nrow(mydata)){
  mpre.matrix<-matrix(0,6,561)
  for(n in 1:561){
    for(q in 1:6){
      if(mydata[m,n]>=mrange[6*q-3,n] & mydata[m,n]<=mrange[6*q-2,n]){
        mpre.matrix[q,n]=3
      }else 
      if(mydata[m,n]>=mrange[6*q-4,n] & mydata[m,n]<=mrange[6*q-1,n]){
        mpre.matrix[q,n]=2
      }else
      if(mydata[m,n]>=mrange[6*q-5,n] & mydata[m,n]<=mrange[6*q  ,n]){
        mpre.matrix[q,n]=1
      }
    }
  }
  G_pre[m]=which.max(apply(mpre.matrix,1,sum))
}


#### conclusions ##################################
sum((mydata$G-G_pre)!=0)/nrow(mydata) #error rate
table(as.matrix(mydata$G), G_pre)
