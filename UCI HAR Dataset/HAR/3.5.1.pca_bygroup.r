#### read data
load("mydata_train.rda")
load("mydata.rda")


## data preprocessing
mydata_train1=(subset(mydata_train, group==1))[,-562]
mydata_train2=(subset(mydata_train, group==2))[,-562]
mydata1=(subset(mydata, group==1))[,-562]
mydata2=(subset(mydata, group==2))[,-562]
G1_train=mydata_train1$G_train
G2_train=mydata_train2$G_train
G1=mydata1$G
G2=mydata2$G
mydata_train1=mydata_train1[,-562]
mydata_train2=mydata_train2[,-562]
mydata1=mydata1[,-562]
mydata2=mydata2[,-562]

nn1=250  #use former nn1 PCs in the first group
nn2=250  #use former nn2 PCs in the second group


#1### principal components analysis ################
train.pr1 = princomp(mydata_train1)


#### principal components regression ##############
pre1=predict(train.pr1)
Fi1=as.data.frame(matrix(0, nrow=nrow(mydata_train1), ncol = nn1))
for(i in 1:nn1){
  Fi1[,i]=pre1[,i]
}
Fi1=cbind(G1_train, Fi1)
colnames(Fi1)=c("G1_train", colnames(pre1)[1:nn1])
## Establish a linear regression model of the former k PCs and G1_train
lm1.sol0=lm(G1_train~., data=Fi1)


#### transformation ###############################
beta1=coef(lm1.sol0)
A1=loadings(train.pr1)
x1.bar=train.pr1$center
x1.sd=train.pr1$scale
if(length(beta1)==2){
  coef1=(beta1[2]*A1[,1])/x1.sd
}
if(length(beta1)>=3){
  coef1=beta1[2]*A1[,1]
  for(i in 2:(length(beta1)-1)){
    coef1=coef1 + beta1[i+1]*A1[,i]
  }
  coef1=coef1/x1.sd
}
beta01=beta1[1]-sum(x1.bar*coef1)
G1_pre=round(beta01 + as.matrix(mydata1)%*%coef1)
G1_pre[G1_pre<=0]=1
G1_pre[G1_pre>=4]=3


#2### principal components analysis ################
train.pr2 = princomp(mydata_train2)


#### principal components regression ##############
pre2=predict(train.pr2)
Fi2=as.data.frame(matrix(0, nrow=nrow(mydata_train2), ncol = nn2))
for(i in 1:nn2){
  Fi2[,i]=pre2[,i]
}
Fi2=cbind(G2_train, Fi2)
colnames(Fi2)=c("G2_train", colnames(pre2)[1:nn2])
## Establish a linear regression model of the former k PCs and G2_train
lm2.sol0=lm(G2_train~., data=Fi2)


#### transformation ###############################
beta2=coef(lm2.sol0)
A2=loadings(train.pr2)
x2.bar=train.pr2$center
x2.sd=train.pr2$scale
if(length(beta2)==2){
  coef2=(beta2[2]*A2[,1])/x2.sd
}
if(length(beta2)>=3){
  coef2=beta2[2]*A2[,1]
  for(i in 2:(length(beta2)-1)){
    coef2=coef2 + beta2[i+1]*A2[,i]
  }
  coef2=coef2/x2.sd
}
beta02=beta2[1]-sum(x2.bar*coef2)
G2_pre=round(beta02 + as.matrix(mydata2)%*%coef2)
G2_pre[G2_pre<=3]=4
G2_pre[G2_pre>=7]=6


#### conclusions ##################################
sum((G1-G1_pre)!=0)/nrow(mydata1)  #error rate
table(as.matrix(G1), G1_pre)
sum((G2-G2_pre)!=0)/nrow(mydata2)  #error rate
table(as.matrix(G2), G2_pre)