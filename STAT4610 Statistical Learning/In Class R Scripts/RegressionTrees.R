################################################################################################
## Simulated data
################################################################################################

library(tree)

set.seed(38)
x <- sort(runif(100,0,10))
y <- cos(x) * exp(-x/5) + rnorm(100,sd=0.1)

dat <- data.frame(x=x,y=y)
plot(dat)

## First tree
fit <- tree(y~x,data=dat)
fit
summary(fit)

x.seq <- seq(0,10,by=0.01)
newdat <- data.frame(x=x.seq)
pred <- predict(fit,newdata=newdat)

plot(dat)
lines(pred~x.seq)

# where are the splits?
these <- which(diff(fit$where)!=0)
abline(v=(x[these+1]+x[these])/2,lty=2)

################################################################################################
## Movies data
################################################################################################

rm(list=ls())

library(tree)

dat <- read.table("~/Documents/Classes/STAT5610data/movies.tab",
  sep="\t", header=TRUE, quote="", comment="")

dat <- dat[,-c(1,2,6:16)]
str(dat)
for(i in 5:11){
  dat[,i] <- factor(dat[,i])
}
str(dat)

## Does budget affect ratings?
out <- tree(rating~budget,data=dat)
plot(out)
text(out)

## Training and testing data
set.seed(33)
these.train <- sample(1:nrow(dat),size=floor(nrow(dat)*0.8))
dat.train <- dat[these.train,]
dat.test <- dat[-these.train,]

## First tree
out <- tree(rating~.,data=dat.train)
out
plot(out)
text(out,pretty=0)
# predict on test data
pred <- predict(out,newdata=dat.test)

mean( (dat.test$rating - pred)^2 )
# compare to training error
mean( (out$y - predict(out))^2 )

## What would the tree fit on the testing data look like?
out <- tree(rating~.,data=dat.test)
dev.new()
plot(out,main="Testing data")
text(out,pretty=0)

##
## Training and testing data: how do predictive fits compare?
##

set.seed(33)

RMSE.train <- RMSE.test <- NULL

N <- 200

for(i in 1:N){
  these.train <- sample(1:nrow(dat),size=floor(nrow(dat)*0.8))
  dat.train <- dat[these.train,]
  dat.test <- dat[-these.train,]
  
  ## Fit tree on training data
  out <- tree(rating~.,data=dat.train)
  
  ## Predict on test data
  pred <- predict(out,newdata=dat.test)

  ## Store statistics
  RMSE.test[i] <- sqrt(mean( (dat.test$rating - pred)^2 ))
  RMSE.train[i] <- sqrt(mean(summary(out)$resid^2))
}

boxplot(RMSE.train,RMSE.test,names=c("Training","Testing"))
mean(RMSE.test)/mean(RMSE.train) # about 8% worse in testing data

##
## Cross-validation to see if pruning tree will improve performance
##

out <- tree(rating~.,data=dat.train)
plot(out)
text(out,pretty=0)

out.cv <- cv.tree(out)
# no pruning necessary, according to CV
# but, could still prune the tree via

out.pruned <- prune.tree(out,best=4)
dev.new()
plot(out.pruned)
text(out.pruned,pretty=0)

################################################################################################
## Just for fun: FCQ data
################################################################################################

rm(list=ls())

library(tree)

dat <- read.csv("~/Documents/Classes/STAT5610data/CUFCQs/CUFCQs20082015.csv",header=TRUE)

dat <- dat[,-c(1:7,10,22:29,31,33:39)]

str(dat)

dat$HoursPerWkInclClass <- as.factor(dat$HoursPerWkInclClass)
dat$College <- as.factor(dat$College)
dat$Level <- as.factor(dat$Level)

str(dat)

#dat <- dat[,c("InstructorOverall","PriorInterest","Availability","Challenge","HowMuchLearned","InstrRespect")]

dat <- dat[complete.cases(dat),]

out <- tree(InstructorOverall~.,data=dat)

plot(out)
text(out)

################################################################################################
## Trees for regression: downscaling MODIS snow cover percent to Landsat scale
## - A set of 1600 x 1600 matrices with the following predictors
##   - asp = aspect
##   - elev = elevation
##   - lty = land type class
##   - slope = slope
##   - LST = Landsat estimate of snow cover percentage in a portion of the South Sierra Nevadas
## - MOD = a low resolution 100 x 100 matrix of MODIS estimates of snow cover percent.
##         Each single MODIS value has 16 x 16 corresponding Landsat values
## The goal is to predict Landsat using the set of features, perhaps include lon/lat
################################################################################################

rm(list=ls())

library(tree)
library(fields)

##
## Load and examine data
##

load("~/Documents/Classes/STAT5610data/SierraBighornExample.RData")

ls()
dim(MOD)
dim(LST)

X <- matrix(1:1600,1600,1600)
Y <- t(matrix(1:1600,1600,1600))

# warning: these plots take awhile to render
#png("SnowCoverData.png",height=800,width=1100)
par(mfrow=c(2,3))
image.plot(MOD,main="MODIS")
image.plot(LST,main="Landsat")
image.plot(slope,main="slope")
image.plot(asp,main="aspect")
image.plot(elev,main="elevation")
#dev.off()

# make MOD same dimension as high resolution images by copying each value into 16x16 matrix
MOD.big <- array(dim=c(1600,1600))
for(k in 1:100){
  for(ell in 1:100){
    MOD.big[(16*k-15):(16*k),(16*ell-15):(16*ell)] <- MOD[k,ell]
  }
}

MOD[1:2,1:2]
MOD.big[1:17,1:17]

dat <- data.frame(LST=c(LST),mod=c(MOD.big),slope=c(slope),asp=c(asp),elev=c(elev),X=c(X),Y=c(Y))

##
## Regression
##

these <- complete.cases(dat)
dat <- dat[these,]
str(dat)

out <- tree(LST~.,data=dat)
out

plot(out)
text(out)

LST.pred <- array(dim=c(1600,1600))
LST.pred[these] <- predict(out)

## Predicted snow covers
#png("SnowCoverTree.png",height=800/2,width=1100)
par(mfrow=c(1,3))
image.plot(MOD,main="MODIS")
image.plot(LST,main="True Landsat")
image.plot(LST.pred,zlim=c(0,100),main="Predicted Landsat")
#dev.off()

