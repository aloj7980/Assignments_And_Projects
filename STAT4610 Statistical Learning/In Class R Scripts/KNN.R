################################################################################################
## A simple classifier: k-nearest neighbors
################################################################################################

library(class)

##
## 2D example
##

n <- 100

set.seed(10)
V0.0 <- rnorm(40,mean=1,sd=0.5)
V0.1 <- rnorm(60,mean=0,sd=0.5)
V1.0 <- rnorm(40,mean=-1,sd=1)
V1.1 <- rnorm(60,mean=0,sd=1)
dat <- data.frame(class=rep(c(0,1),times=c(40,60)), V0=c(V0.0,V0.1), V1=c(V1.0,V1.1))

plot(V1~V0,data=dat,asp=1,type="n")
points(V1~V0,data=dat,subset=dat$class==1,col="red",pch=19)
points(V1~V0,data=dat,subset=dat$class==0,col="blue",pch=19)
legend(x="topright",legend=c("0","1"),pch=c(19,19),col=c("blue","red"))

# or maybe better
plot(V1~V0,data=dat,asp=1,type="n")
text(V1~V0,data=dat,subset=dat$class==1,labels="1")
text(V1~V0,data=dat,subset=dat$class==0,labels="0")

## Predictions on a grid
predict.grid <- as.matrix(expand.grid(seq(-4,4,length.out=150),seq(-4,4,length.out=150)))
dim(predict.grid)
points(predict.grid,col="blue")

## Predictions for k=1
k <- 1
plot(V1~V0,data=dat,asp=1,main=paste("# of nearest neighbors =",k),type="n")
text(V1~V0,data=dat,subset=dat$class==1,labels="1",col="red")
text(V1~V0,data=dat,subset=dat$class==0,labels="0",col="blue")

out <- knn(train=dat[,2:3],test=predict.grid,cl=dat[,1],k=k)
points(predict.grid[out==0,],col="blue")

#par(mfrow=c(1,1),ask=TRUE)
#for(k in 1:50){
#  plot(V1~V0,data=dat,asp=1,main=paste("# of nearest neighbors =",k))
#  points(V1~V0,data=dat,subset=dat$class==1,col="red",pch=19)
#  points(V1~V0,data=dat,subset=dat$class==0,col="blue",pch=19)
#
#  out <- knn(train=dat[,2:3],test=predict.grid,cl=dat[,1],k=k)
#  points(predict.grid[out==0,],col="blue")
#  #points(predict.grid[out==1,],col="black",pch=19,cex=0.1)
#}

par(mfrow=c(1,1))
for(k in 1:50){
  plot(V1~V0,data=dat,asp=1,main=paste("# of nearest neighbors =",k),type="n")
  text(V1~V0,data=dat,subset=dat$class==1,labels="1",col="red")
  text(V1~V0,data=dat,subset=dat$class==0,labels="0",col="blue")

  out <- knn(train=dat[,2:3],test=predict.grid,cl=dat[,1],k=k)
  points(predict.grid[out==0,],col="blue")
  Sys.sleep(0.3)
}

################################################################################################
## Finding k via cross-validation
################################################################################################

library(class)

dat <- read.csv("~/Documents/Classes/Fall2023/5610/data/bank/bank.csv",header=TRUE,sep=";")

## Set up training/testing samples, approximately 50% split
set.seed(38383)
n <- dim(dat)[1]
train <- sample(c(FALSE,TRUE),size=n,replace=TRUE)
dat.train <- dat[train,c("age","duration","y")]
dat.test <- dat[!train,c("age","duration","y")]

# coerce to log(duration)
dat.train[,2] <- log(dat.train[,2])
dat.test[,2] <- log(dat.test[,2])

plot(dat.train[,2:1],xlab="log(duration)",type="n") # 2:1 so this matches DiscriminantAnalysis.R
points(dat.train[dat.train[,3]=="no",2:1],col="red",pch=19,cex=0.5)
points(dat.train[dat.train[,3]=="yes",2:1],col="blue",pch=19,cex=0.5)

par(mfrow=c(2,2))
for(k in c(1,2,5,10)){
  out <- knn(train=dat.train[,1:2],test=dat.test[,1:2],cl=dat.train[,3],k=k)
  plot(dat.train[,2:1],type="n",xlab="log(duration)",main=paste("k=",k))
  points(dat.test[out=="no",2:1],col="red",pch=19,cex=0.5)
  points(dat.test[out=="yes",2:1],col="blue",pch=19,cex=0.5)
}

## Leave-one-out cross-validation
error.rate <- rep(NA,20)
for(k in 1:20){
  out <- knn.cv(train=dat.train[,1:2],cl=dat.train[,3],k=k)
  error.rate[k] <- mean(out != dat.train[,3])
}
plot(error.rate,type="b") # maybe around 10?

## Plots on testing data
par(mfrow=c(2,2))
plot(dat.train[,2:1],type="n",xlab="log(duration)",main="Training")
points(dat.train[dat.train[,3]=="no",2:1],cex=0.5,pch=19,col="red")
points(dat.train[dat.train[,3]=="yes",2:1],cex=0.5,pch=19,col="blue")
k <- 1
out <- knn(train=dat.train[,1:2],test=dat.test[,1:2],cl=dat.train[,3],k=k)
plot(dat.train[,2:1],type="n",xlab="log(duration)",main=paste("k=",k))
points(dat.test[out=="no",2:1],col="red",pch=19,cex=0.5)
points(dat.test[out=="yes",2:1],col="blue",pch=19,cex=0.5)
k <- 10
out <- knn(train=dat.train[,1:2],test=dat.test[,1:2],cl=dat.train[,3],k=k)
plot(dat.train[,2:1],type="n",xlab="log(duration)",main=paste("k=",k))
points(dat.test[out=="no",2:1],col="red",pch=19,cex=0.5)
points(dat.test[out=="yes",2:1],col="blue",pch=19,cex=0.5)
plot(dat.test[,2:1],type="n",xlab="log(duration)",main="Testing")
points(dat.test[dat.test[,3]=="no",2:1],cex=0.5,pch=19,col="red")
points(dat.test[dat.test[,3]=="yes",2:1],cex=0.5,pch=19,col="blue")

## Error rate on testing data
k <- 1
out <- knn(train=dat.train[,1:2],test=dat.test[,1:2],cl=dat.train[,3],k=k)
mean(out != dat.test[,3])
k <- 10
out <- knn(train=dat.train[,1:2],test=dat.test[,1:2],cl=dat.train[,3],k=k)
mean(out != dat.test[,3])
