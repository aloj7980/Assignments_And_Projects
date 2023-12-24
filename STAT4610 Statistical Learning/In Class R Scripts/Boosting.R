################################################################################################
## Exploring the boosting idea
################################################################################################

library(rpart)

set.seed(38)
x <- sort(runif(100,0,10))
y <- cos(x) * exp(-x/5) + rnorm(100,sd=0.1)

x.seq <- seq(0,10,by=0.01)
newdat <- data.frame(x=x.seq)

dat <- data.frame(x=x,y=y)
plot(dat)

## First step of the algorithm
f.hat <- rep(0,length(x.seq))
dat.tmp <- dat
lambda <- 0.1
par(mfrow=c(2,1))
fit <- rpart(y~x,data=dat.tmp,control=list(maxdepth=1))
pred <- predict(fit,newdata=newdat)
f.hat <- f.hat + lambda * pred
plot(dat.tmp,ylim=range(dat$y),col="grey",pch=19,cex=0.5)
lines(x.seq,pred)
dat.tmp$y <- dat.tmp$y - lambda * predict(fit)
plot(dat)
points(dat.tmp,col="grey",pch=19,cex=0.5)
lines(x.seq,f.hat)

## Movie
f.hat <- rep(0,length(x.seq))
dat.tmp <- dat
lambda <- 0.1
par(mfrow=c(2,1))
for(i in 1:100){
  fit <- rpart(y~x,data=dat.tmp,control=list(maxdepth=1))
  pred <- predict(fit,newdata=newdat)
  f.hat <- f.hat + lambda * pred
  plot(dat.tmp,ylim=range(dat$y),main="Current step",col="grey",pch=19,cex=0.5)
  lines(x.seq,pred)
  dat.tmp$y <- dat.tmp$y - lambda * predict(fit)
  plot(dat,main="Fitted function")
  lines(x.seq,f.hat)
  Sys.sleep(0.2)
}

## Run it for a long time
f.hat <- rep(0,length(x.seq))
dat.tmp <- dat
lambda <- 0.1
for(i in 1:10000){
  fit <- rpart(y~x,data=dat.tmp,control=list(maxdepth=1))
  pred <- predict(fit,newdata=newdat)
  f.hat <- f.hat + lambda * pred
  dat.tmp$y <- dat.tmp$y - lambda * predict(fit)
}

dev.off()

plot(dat)
lines(x.seq,f.hat)

rm(list=ls())

################################################################################################
## Movies data
################################################################################################

library(gbm)

dat <- read.table("~/Documents/Classes/STAT5610data/movies.tab",
  sep="\t", header=TRUE, quote="", comment="")

dat <- dat[,-c(1,2,6:17)]
for(i in 4:10){
  dat[,i] <- factor(dat[,i])
}
str(dat)

## Boosted regression tree
set.seed(1)
out <- gbm(rating~.,data=dat,n.trees=500,interaction.depth=3,shrinkage=0.01)

out

summary(out)

# partial dependence plot: integrates out other variables
plot(out, i="length")
plot(out, i="budget")

################################################################################################
## FCQ data
################################################################################################

rm(list=ls())

dat <- read.csv("~/Documents/Classes/STAT5610data/CUFCQs/CUFCQs20082015.csv",header=TRUE)

dat <- dat[,-c(1:7,10,22:29,31,33:39)]

dat$HoursPerWkInclClass <- as.factor(dat$HoursPerWkInclClass)
dat$College <- as.factor(dat$College)
dat$Level <- as.factor(dat$Level)
dat <- dat[complete.cases(dat),]

str(dat)

## Boosted regression tree
set.seed(1)
out <- gbm(InstructorOverall~.,data=dat,n.trees=5000,interaction.depth=1,shrinkage=0.01,
  distribution="gaussian",cv.folds=5)

out
summary(out)

summary(out,cBars=5,method=relative.influence)
summary(out,cBars=5,method=permutation.test.gbm)

# partial dependence plot
plot(out, i="InstrEffective")
plot(out, i="CourseOverall")
plot(out, i="Availability")
plot(out, i="InstrRespect")

## Some summaries
names(out)
sqrt(min(out$cv.error))
gbm.perf(out,method="cv") # estimate number of boosting iterations needed

################################################################################################
## Compare to a random forest on the snow cover data
################################################################################################

rm(list=ls())

library(xgboost)
library(ranger)
library(fields)

load("~/Documents/Classes/STAT5610data/SierraBighornExample.RData")

# see images from RegressionTrees script!

## This is lazy, but a first step: put the 100x100 MODIS at the same resolution as
## Landsat by copying in each value to a 16x16 matrix
MOD.big <- array(dim=c(1600,1600))
for(k in 1:dim(MOD)[1]){
  for(ell in 1:dim(MOD)[2]){
    MOD.big[(16*k-15):(16*k),(16*ell-15):(16*ell)] <- MOD[k,ell]
  }
}

dat <- data.frame(asp=c(asp),elev=c(elev),slope=c(slope),MOD.big=c(MOD.big),LST=c(LST))

set.seed(3820)
train <- sample(c(TRUE,FALSE),size=1600^2,replace=TRUE)
train[!complete.cases(dat)] <- FALSE
theseNA <- !complete.cases(dat)
sum(theseNA)
# not too many NAs, let's just impute all values the mean, a very lazy and uninspired fix

for(i in 1:5){
  dat[theseNA,i] <- mean(dat[!theseNA,i])
}

## Random forest
fit.rf <- ranger(LST~.,data=dat[train,],num.trees=100)

## Boosted regression tree
# gbm is prohibitively slow on these data
#fit.gbm <- gbm(LST~.,data=dat[train,],distribution="gaussian",n.trees=5000,
#  interaction.depth=1,shrinkage=0.01,cv.folds=5,verbose=TRUE)

dat.tmp <- data.matrix(dat[train,-5])
lst.tmp <- as.numeric(dat[train,5])
xgb.train <- xgb.DMatrix(data = dat.tmp, label = lst.tmp)

dat.tmp <- data.matrix(dat[!train,-5])
lst.tmp <- as.numeric(dat[!train,5])
xgb.test <- xgb.DMatrix(data = dat.tmp, label = lst.tmp)

fit.xgboost <- xgboost(data=xgb.train,max.depth=3,nrounds=1000)

## Predict on testing data 
pred.rf <- predict(fit.rf,data=dat[!train,])
pred.xg <- predict(fit.xgboost,xgb.test)
# RMSE
sqrt( mean( ( dat$LST[!train] - pred.rf$predictions )^2 ) ) # 13.326
sqrt( mean( ( dat$LST[!train] - pred.xg )^2 ) ) # 14.521

