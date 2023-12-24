################################################################################################
## Movies data
################################################################################################

rm(list=ls())

library(tree)

dat <- read.table("~/Documents/Classes/STAT5610data/movies.tab",
  sep="\t", header=TRUE, quote="", comment="")

dat <- dat[,-c(1,2,6:16)]
for(i in 5:11){
  dat[,i] <- factor(dat[,i])
}
str(dat)

##
## Bagging to improve model predictions
##

## Training and testing data
set.seed(33)
these.train <- sample(1:nrow(dat),size=floor(nrow(dat)*0.8))
dat.train <- dat[these.train,]
dat.test <- dat[-these.train,]

## Single tree
out <- tree(rating~.,data=dat.train)
# predict on test data and check MSE
pred <- predict(out,newdata=dat.test)
mean( (dat.test$rating - pred)^2 ) # out of sample MSE
mean(summary(out)$resid^2) # in sample MSE

## Bagging
N <- 500
PRED.boot <- matrix(nr=length(dat.test$rating),nc=N)

set.seed(180)
for(i in 1:N){
  bag.indices <- sample(1:dim(dat.train)[1],size=dim(dat.train)[1],replace=TRUE)
  out <- tree(rating~.,data=dat.train[bag.indices,])
  PRED.boot[,i] <- predict(out,newdata=dat.test)
}
# average the predictions from the bootstrap-resampled data tree fits
PRED.bagged <- apply(PRED.boot,1,mean)

mean( (dat.test$rating - pred)^2 )
mean( (dat.test$rating - PRED.bagged)^2 )

################################################################################################
## Snow cover percentage data
################################################################################################

rm(list=ls())

library(randomForest)
library(fields)

##
## Load and examine data
##

load("~/Documents/Classes/STAT5610data/SierraBighornExample.RData")

X <- matrix(1:1600,1600,1600)
Y <- t(matrix(1:1600,1600,1600))

# make MOD same dimension as high resolution images by copying each value into 16x16 matrix
MOD.big <- array(dim=c(1600,1600))
for(k in 1:100){
  for(ell in 1:100){
    MOD.big[(16*k-15):(16*k),(16*ell-15):(16*ell)] <- MOD[k,ell]
  }
}

dat <- data.frame(LST=c(LST),mod=c(MOD.big),slope=c(slope),asp=c(asp),elev=c(elev),
  X=c(X),Y=c(Y),lty=as.factor(c(lty)))

##
## Bagging to improve predictions
##

these <- complete.cases(dat)
dat <- dat[these,]

set.seed(223)
train <- sample(1:nrow(dat),size=1e4)

# bagging
set.seed(2)
bag <- randomForest(LST~.,data=dat,subset=train,mtry=dim(dat)[2]-1,importance=TRUE,ntree=50)

LST.pred <- array(dim=c(1600,1600))
LST.pred[these] <- predict(bag,dat)

## Predicted snow covers
#png("SnowCoverBag.png",height=800/2,width=1100)
par(mfrow=c(1,3))
image.plot(MOD,main="MODIS")
image.plot(LST,main="Landsat")
image.plot(LST.pred,zlim=c(0,100),main="Predicted Landsat")
#dev.off()

sqrt(mean( (LST.pred - LST)^2, na.rm=TRUE))

# compare against a single tree
library(tree)
one.tree <- tree(LST~.,data=dat,subset=train)

sqrt(mean( (predict(one.tree) - LST)^2, na.rm=TRUE))

## Variable importances
?importance
importance(bag)
varImpPlot(bag)
