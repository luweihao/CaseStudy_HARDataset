#### read data 
load("SamsungData.train.rda")
load("SamsungData.test.rda")
mydata_train = SamsungData_train[,1:561]
colnames(mydata_train)=1:561
G_train=SamsungData_train$activity_labels
mydata = SamsungData[,1:561]
colnames(mydata)=1:561
G=SamsungData$activity_labels

nn=250  #choose the former k PCs

#### principal components analysis ################
train.pr = princomp(mydata_train)

 # We want to get the accumulated contribution rate,
 # so as to choose k principal components to run the regression analysis,
 # and the following 4 lines can do so(warming:the printing is a bit great)
if(0){
  op=options()
  options(max.print=10000)
  print(summary(train.pr))
  options(op)
}

##The printing result & running result is as follow
#                         Comp.34     Comp.67      Comp.155      Comp.368    Comp.419     Comp.438
#Standard deviation     0.362335576 0.2304491869 0.0983377406 1.353945e-02
#Proportion of Variance 0.002358659 0.0009540992 0.0001737335 3.293407e-06
#Cumulative Proportion  0.900937088 0.9504592206 0.9901114630 9.999026e-01 9.999904e-01 9.999990e-01
#Error rate             0.3196471   0.2354937    0.1812012    0.1543943    0.1465898    0.1465898
if(0){
  x=c(34,67,155,368,419,438)
  y=c(0.3196471,0.2354937,0.1812012,0.1543943,0.1465898,0.1465898)
  plot(x, y, type = "b", xlab="k PCs", ylab = "Error rate")
}

##the following code in annotation may cost long time to run, and it will return
 #the suggested k, the number of principal components we advised to choose, and
 #plot the correltion between the heading 10 PCs and origin Xis
if(0){
  library(psych)
  fa.parallel(mydata_train, fa = "pc", n.iter = 10, 
              show.legend = FALSE, main = "Scree plot with parallel analysis")
  #set parameter n.iter=10(it's advisable to have a larger n.iter)
  #"In factor.scores, the correlation matrix is singular, an approximation is used"
  #Parallel analysis suggests that the number of components =  44
  train.pr2=principal(mydata_train, nfactors=10, rotate="none")
  fa.diagram(train.pr2)
}

#### principal components regression ##############
pre=predict(train.pr)
Fi=as.data.frame(matrix(0, nrow=nrow(mydata_train), ncol = nn))
for(i in 1:nn){
  Fi[,i]=pre[,i]
}
Fi=cbind(G_train, Fi)
colnames(Fi)=c("G_train", colnames(pre)[1:nn])
## Establish a linear regression model of the former k PCs and G_train
lm.sol0=lm(G_train~., data=Fi)


#### transformation ###############################
beta=coef(lm.sol0)
A=loadings(train.pr)
x.bar=train.pr$center
x.sd=train.pr$scale
if(length(beta)==2){
  coef=(beta[2]*A[,1])/x.sd
}
if(length(beta)>=3){
  coef=beta[2]*A[,1]
  for(i in 2:(length(beta)-1)){
    coef=coef + beta[i+1]*A[,i]
  }
  coef=coef/x.sd
}
beta0=beta[1]-sum(x.bar*coef)
G_pre=round(beta0 + as.matrix(mydata)%*%coef)
G_pre[G_pre<=0]=1
G_pre[G_pre>=7]=6


#### conclusions ##################################
sum((G-G_pre)!=0)/nrow(mydata) #error rate
table(as.matrix(G), G_pre)
