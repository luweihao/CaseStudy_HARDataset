distinguish.bayes=function(TrnX, TrnG, p=rep(1, length(levels(TrnG))),
                           TstX = NULL, TstG = NULL, var.equal = FALSE)
{
   flag=0
   if (is.null(TstX) == TRUE){
     TstX=TrnX
     flag=1
   }
   if (is.vector(TstX) == TRUE)  TstX=t(as.matrix(TstX))
   if (is.matrix(TstX) != TRUE)
      TstX=as.matrix(TstX)
   if (is.matrix(TrnX) != TRUE) TrnX=as.matrix(TrnX)

   nx=nrow(TstX)
   blong=matrix(rep(0, nx), nrow=1, dimnames=list("blong", 1:nx))
   g=length(levels(TrnG))
   
   mu=matrix(0, nrow=g, ncol=ncol(TrnX))   
   for (i in 1:g)
   {
   mu[i,]=colMeans(TrnX[TrnG==i,])
   } 
   
   D=matrix(0, nrow=g, ncol=nx)
   if (var.equal == TRUE)
   {
      for (i in 1:g)
        {
         d2=mahalanobis(TstX, mu[i,], var(TrnX))
         D[i,]=d2 - 2*log(p[i])
        }
   }else{
      for (i in 1:g)
      {
         S=var(TrnX[TrnG==i,])
         d2= mahalanobis(TstX, mu[i,], S)
         D[i,]=d2 - 2*log(p[i])-log(det(S))
      }
   }
  
   for (j in 1:nx)
   {
      dmin=Inf
      for (i in 1:g)
          if (D[i,j]<dmin)
          {
          dmin=D[i,j]; blong[j]=i
          }
   }
   if(flag){
     table1 <- as.data.frame( t(rbind(TrnG, blong)) )
     print(xtabs(~ TrnG+blong, data = table1))
     #wrong <- (1:nx)[(as.numeric(TrnG)-as.numeric(blong))!=0]
     #cat("The index of wrong data are:", wrong, "\n")
     return(table1)
   }else{
     origin = TstG
     table1 <- as.data.frame( t(rbind(origin, blong)) )
     print(table(table1))
     #wrong <- (1:nx)[(as.numeric(origin)-as.numeric(blong))!=0]
     #cat("The index of wrong data are:", wrong, "\n")
     return(table1)
   }
}

