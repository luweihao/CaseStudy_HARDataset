pca.choose=function(mydata_train.ori, mydata.ori, groupi, nnc, printtable=FALSE, fixed=FALSE)
{
  ## data preprocessing
  ncols=ncol(mydata.ori)-1
  mydata_traini=(subset(mydata_train.ori, group==groupi))[,-ncols]
  mydatai=(subset(mydata.ori, group==groupi))[,-ncols]
  G_train=mydata_traini$G_train
  G=mydatai$G
  mydata_train=mydata_traini[,-ncols]
  mydata=mydatai[,-ncols]
  
  
  #### principal components analysis ################
  train.pr = princomp(mydata_train)
  pre=predict(train.pr)
  
  acc=matrix(0, nrow = 2, ncol=length(nnc))
  acc[1,]=nnc
  for(j in 1:length(nnc)){
    nn=nnc[j]
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
    G_pre[G_pre<=(3*groupi-3)]=3*groupi-2
    if(!fixed){G_pre[G_pre>=(3*groupi+1)]=3*groupi}
    if(fixed){G_pre[G_pre>=3*groupi]=3*groupi-1}
    acc[2,j]<-sum((G-G_pre)!=0)/nrow(mydata)
    if(printtable){print(table(as.matrix(G), G_pre))}
  }

  return(list(group=groupi, acc=acc))
}
