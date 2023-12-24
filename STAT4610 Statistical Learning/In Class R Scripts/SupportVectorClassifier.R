################################################################################################
## Support vector classifiers
################################################################################################

library(e1071)

N <- 500

set.seed(9)
X <- matrix(rnorm(N*2),ncol=2)
y <- rep(c(-1,1),each=N/2)
X[y==1,] = X[y==1,] + 1.5

col <- rep(NA,N)
col[y==1] <- rainbow(2)[1]
col[y==-1] <- rainbow(2)[2]

col

pch <- rep(NA,N)
pch[y==1] <- 16
pch[y==-1] <- 17

plot(X[,c(2,1)],col=col,pch=pch)

dat <- data.frame(X=X,y=as.factor(y))
# note that coercing y to be a factor is important for svm to interpret as a classification!
fit <- svm(y~.,data=dat,kernel="linear",cost=0.1,scale=FALSE)
fit
plot(fit,data=dat)
# support vectors = "x" (otherwise "o")
# C-classifier is the same as nu-classifier but with rescaling on cost parameter:
# C > 0 and nu in (0,1)

dev.off()
fit <- svm(y~.,data=dat,kernel="linear",cost=0.01,scale=FALSE)
plot(fit,data=dat)
dev.new()
fit <- svm(y~.,data=dat,kernel="linear",cost=1,scale=FALSE)
plot(fit,data=dat)

head(fit$SV) # support vectors (how many are there?)
plot(X[,c(2,1)],col=col,pch=pch)
points(fit$SV[,c(2,1)])
# or
plot(fit,data=dat,fill=FALSE)
points(fit$SV[,c(2,1)])

## Tuning the cost parameter (10-fold CV)
tune.out <- tune(svm,y~.,data=dat,kernel="linear",scale=FALSE,
  ranges=list(cost=seq(0.01,5,length.out=100)))

names(tune.out)
tune.out$sampling
tune.out$best.parameters

best.model <- tune.out$best.model

best.model
is(best.model)

plot(best.model,data=dat,xlim=c(-2,3),ylim=c(-2,3))

## Which points not classified well?
dev.new()
plot(X[,c(2,1)],col=col,pch=pch,xlim=c(-2,3),ylim=c(-2,3))
these <- which(best.model$fitted != y)
points(X[these,c(2,1)],cex=1.5)

grd <- expand.grid(seq(-4,4,0.05),seq(-4,4,0.05))
names(grd) <- c("X.1","X.2")
newpred <- predict(best.model,newdat=grd)
points(grd[,c(2,1)][newpred==1,],col=rainbow(2)[1],pch=3,cex=0.1)
points(grd[,c(2,1)][newpred==-1,],col=rainbow(2)[2],pch=3,cex=0.1)

##
## A poor SVC
##

rm(list=ls())

set.seed(10)
X <- matrix(rnorm(40*2),ncol=2)
y <- c(rep(-1,10),rep(1,20),rep(-1,10))
X[11:30,] <- X[11:30,] + 2
X[31:40,] <- X[31:40,] + 4

col <- rep(NA,40)
col[y==1] <- rainbow(2)[1]
col[y==-1] <- rainbow(2)[2]

pch <- rep(NA,40)
pch[y==1] <- 16
pch[y==-1] <- 17

plot(X[,c(2,1)],col=col,pch=pch,cex=2)

dat <- data.frame(X=X,y=as.factor(y))

fit <- svm(y~.,data=dat,kernel="linear",cost=0.001)
plot(fit,data=dat)
fit <- svm(y~.,data=dat,kernel="linear",cost=100)
plot(fit,data=dat)

