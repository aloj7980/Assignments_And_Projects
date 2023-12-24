################################################################################################
## Random forests for regression: simple example
## But, also kindof stupid because random forests are meant for high-dimensional feature space
################################################################################################

library(ranger)
library(tree)

set.seed(38)
x <- sort(runif(100,0,10))
y <- cos(x) * exp(-x/5) + rnorm(100,sd=0.1)

dat <- data.frame(x=x,y=y)
plot(dat)

## Regular 'ol tree
tree1 <- tree(y~x,data=dat)

## Random forest
rf <- ranger(y~x,data=dat)
# note this is really just a bagged predictor since p = 1

## Predict and plot
x.seq <- seq(0,10,by=0.01)
newdat <- data.frame(x=x.seq)
pred.rf <- predict(rf,data=newdat)
pred.tree <- predict(tree1,newdata=newdat)

plot(dat)
lines(pred.tree~x.seq)
lines(pred.rf$predictions~x.seq,col="red",lwd=2)

################################################################################################
## Random forests for regression: downscaling MODIS snow cover percent to Landsat scale
## - A set of 1600 x 1600 matrices with the following predictors
##   - asp = aspect
##   - elev = elevation
##   - lty = land type class
##   - slope = slope
##   - LST = Landsat estimate of snow cover percentage in a portion of the South Sierra Nevadas
## - MOD = a low resolution 100 x 100 matrix of MODIS estimates of snow cover percent.
##         Each single MODIS value has 16 x 16 corresponding Landsat values
## The goal is to predict Landsat using the set of 5 features
################################################################################################

rm(list=ls())

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

## Just some exploratory plots
par(mfrow=c(2,3))
hist(LST)
hist(MOD)
hist(slope)
hist(lty)
hist(elev)
hist(asp)

## Note...
mean(LST == 100,na.rm=TRUE)
mean(LST == 0,na.rm=TRUE)
# and the rest are in (0,100)

##
## Try a random forest model
##

dat <- data.frame(asp=c(asp),elev=c(elev),slope=c(slope),MOD.big=c(MOD.big),LST=c(LST))

set.seed(3820)
train <- sample(c(TRUE,FALSE),size=1600^2,replace=TRUE)
train[!complete.cases(dat)] <- FALSE
theseNA <- !complete.cases(dat)

fit <- ranger(LST~.,data=dat[train,],num.trees=100)

## Predict
pred <- predict(fit,data=dat[!theseNA,])
sqrt( mean( ( LST[!theseNA] - pred$predictions )^2 ) ) # RMSE

LST.pred <- matrix(NA,1600,1600)
LST.pred[!theseNA] <- pred$predictions

#png("SnowCoverRF.png",height=800/2,width=1100)
par(mfrow=c(1,3))
image.plot(MOD,main="MODIS")
image.plot(LST,main="True Landsat")
image.plot(LST.pred,main="Predicted Landsat, RF")
#dev.off()

################################################################################################
## Random forests for classification
################################################################################################

rm(list=ls())

library(tree)
library(ranger)

load("~/Documents/Classes/STAT5610data/spam.RData")

ls()

names(dat)

str(dat)

##
## Logistic regression on everything
##

fit.lm <- glm(spam~.,data=dat,family=binomial,subset=train)

pred.lm <- predict(fit.lm,newdata=dat[!train,],type="response")

##
## Bagging classification trees
##

set.seed(1)
bag.fit <- ranger(spam~.,dat=dat[train,],mtry=dim(dat)[2]-1)
pred.bag <- predict(bag.fit,data=dat[!train,])

##
## Random forest
##

fit.rf <- ranger(spam~.,data=dat[train,],probability=TRUE)

fit.rf

pred.rf <- predict(fit.rf,data=dat[!train,])
names(pred.rf)

head(pred.rf$predictions)

pred.rf <- pred.rf$predictions[,1]

## Brier scores
mean( (dat$spam[!train] - pred.lm)^2 )
mean( (dat$spam[!train] - pred.bag$predictions)^2 )
mean( (dat$spam[!train] - pred.rf)^2 )

##
## Variable importance measures
##

# Nembrini et al (2018)
fit.rf <- ranger(spam~.,data=dat[train,],probability=TRUE,importance="impurity_corrected")
cbind(sort(importance(fit.rf)))

# Breiman (2001)
fit.rf2 <- ranger(spam~.,data=dat[train,],probability=TRUE,importance="permutation")
cbind(sort(importance(fit.rf2)))

plot(importance(fit.rf),importance(fit.rf2),type="n")
text(importance(fit.rf),importance(fit.rf2),labels=names(importance(fit.rf)),cex=0.5)
