#### read data
load("mydata_train.rda")
load("mydata.rda")
source("pcafunction.r")
acc1=(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=1, nnc=seq(20,300,10)))$acc
acc2=(pca.choose(mydata_train.ori=mydata_train, mydata.ori=mydata,
                 groupi=2, nnc=seq(20,300,10)))$acc

## acc: the wrong rate
acc=cbind(t(acc1), t(acc2)[,2])
colnames(acc)=c("k", "G1", "G2")


#### plot G-k graph
jpeg("bestk.jpeg", height=500, width = 600, quality = 100)
with(acc, {
  plot(k, G1, pch = 20, col = "red", ylab = "G1 & G2", ylim = c(0.05,0.3));
  lines(lowess(k, G1), col = "red", lwd =2, lty = 1);
  points(k, G2, pch = 19, col = "blue");
  lines(lowess(k, G2), col = "blue", lwd =2, lty = 1);
  legend(250, 0.25, pch = c(20, 19), col = c("red", "blue"), legend = c("G1", "G2"))
})
dev.off()


#### write the result
write.csv(acc,file="acc.csv")

