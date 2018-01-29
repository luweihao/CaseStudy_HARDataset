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


#### principal components analysis ################
train.pr1 = princomp(mydata_train1)
pre1=predict(train.pr1)


#### principal components regression ##############
nn1=260
Fi1=as.data.frame(matrix(0, nrow=nrow(mydata_train1), ncol = nn1))
for(i in 1:nn1){
  Fi1[,i]=pre1[,i]
}
Fi1=cbind(G1_train, Fi1)
colnames(Fi1)=c("G1_train", colnames(pre1)[1:nn1])
## Establish a linear regression model of the former k PCs and G1_train
lm1.sol0=lm(G1_train~., data=Fi1)
beta1=coef(lm1.sol0)

if(1){ ##variables selecting
  ##warning:this block may cost a long time
  formula0x=paste(colnames(lm1.sol0$model)[-c(1,(nn1-48):(nn1+1))], collapse = "+")
  #step1<-step(lm1.sol0, direction = "backward", trace = 0, steps = 20)
  step1<-step(lm1.sol0, direction = "backward", scope=list(lower=c("~",formula0x)), trace = 0)
  formulax=paste(colnames(step1$model)[-1], collapse = "+")
  formula=paste("G1_train", formulax, sep = "~")
  lm1.sol.step=lm(formula, data=Fi1)
  beta1=coef(lm1.sol.step)
}

#### transformation ###############################
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


#### conclusion ###################################
cat("The number of remained variables is", length(beta1)-1, "\n")
sum((G1-G1_pre)!=0)/nrow(mydata1)
table(as.matrix(G1), G1_pre)
