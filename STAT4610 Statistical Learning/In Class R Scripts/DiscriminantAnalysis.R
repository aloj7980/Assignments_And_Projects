################################################################################################
## Discriminant Analysis: simulated data
################################################################################################

library(MASS)
library(fields)

set.seed(10)
V0.0 <- rnorm(40,mean=1,sd=0.5)
V0.1 <- rnorm(60,mean=0,sd=0.5)
V1.0 <- rnorm(40,mean=-1,sd=1)
V1.1 <- rnorm(60,mean=0,sd=1)
dat <- data.frame(class=rep(c(0,1),times=c(40,60)), V0=c(V0.0,V0.1), V1=c(V1.0,V1.1))

plot(V1~V0,data=dat,asp=1)
points(V1~V0,data=dat,subset=dat$class==1,col="red",pch=19)

##
## LDA
##

lda.fit <- lda(class~.,data=dat)
lda.fit
# put group means on plot
points(lda.fit$means,pch="+",cex=2)

# use the coefficients of linear discriminants to predict a hypothetical point(s)
trial1 <- c(1,-2)
trial2 <- c(-0.5,1)
text(rbind(trial1,trial2),labels=c("1","2"),cex=1.5)

sum(lda.fit$scaling * trial1) # very negative => probably group 0
sum(lda.fit$scaling * trial2) # very positive => probably group 1

plot(lda.fit) # histograms of the linear discriminants for the training sample

lda.pred <- predict(lda.fit,dat)
names(lda.pred)
?predict.lda

head(lda.pred$class)
head(lda.pred$posterior)
head(lda.pred$x)

quilt.plot(cbind(dat$V0,dat$V1),lda.pred$x)

# confusion matrix
table(lda.pred$class,dat$class)
mean(lda.pred$class == dat$class)

# note we can get the total number of classifications by thresholding at 50%
sum(lda.pred$posterior[,1] > 0.5)
sum(lda.pred$class == 0)

## Plot the discriminant regions
newdat <- data.frame(expand.grid(seq(-3,3,length.out=50),seq(-3,3,length.out=50)))
names(newdat) <- c("V0","V1")
new.pred <- predict(lda.fit,newdat)

plot(V1~V0,data=dat,pch=19,col="white")
points(newdat[new.pred$class==1,],pch=3,cex=0.5,col="red")
#points(newdat[new.pred$class==0,],pch=3,cex=0.5)

points(V1~V0,data=dat,subset=dat$class==1,col="red",pch=19,cex=0.7)
points(V1~V0,data=dat,subset=dat$class==0,pch=19,cex=0.7)

##
## QDA
##

qda.fit <- qda(class~.,data=dat)
qda.fit

qda.pred <- predict(qda.fit,dat)
names(qda.pred)

table(qda.pred$class,dat$class)
mean(qda.pred$class == dat$class)

## Plot the discriminant regions
new.pred <- predict(qda.fit,newdat)

#plot(V1~V0,data=dat,pch=19,col="white")
points(newdat[new.pred$class==1,],pch=5,cex=0.5,col="grey")
#points(newdat[new.pred$class==0,],pch=3,cex=0.5)

#points(V1~V0,data=dat,subset=dat$class==1,col="red",pch=19,cex=0.7)
#points(V1~V0,data=dat,subset=dat$class==0,pch=19,cex=0.7)

par(mfrow=c(1,2))
quilt.plot(cbind(newdat$V0,newdat$V1),new.pred$posterior[,1])
quilt.plot(cbind(newdat$V0,newdat$V1),new.pred$posterior[,2])

##
## Compare lda/qda fits with ROC curves
##

library(pROC)

r.lda <- roc(dat$class~lda.pred$posterior[,2])
r.qda <- roc(dat$class~qda.pred$posterior[,2])

dev.off()
plot(r.lda)
plot(r.qda,add=TRUE,col="red")

auc(r.lda)
auc(r.qda)

################################################################################################
## Classifying bank term deposits subscriptions (Moro et al 2011 and 2014)
################################################################################################

rm(list=ls())

library(MASS)

dat <- read.csv("~/Documents/Classes/Fall2023/5610/data/bank/bank.csv",header=TRUE,sep=";")
names(dat)
str(dat)

set.seed(38383)
n <- dim(dat)[1]
train <- sample(c(FALSE,TRUE),size=n,replace=TRUE)

y.test <- dat$y[!train]

##
## Need covariates to be approximately normally distributed
##

plot(age~duration,data=dat)
plot(age~log(duration),data=dat)
plot(age~log(duration),data=dat,type="n")
points(age~log(duration),data=dat,subset=dat$y=="no",col="red",pch=19,cex=0.5)
points(age~log(duration),data=dat,subset=dat$y=="yes",col="blue",pch=19,cex=0.5)
# would our assumptions to do LDA or QDA hold here?

##
## LDA
##

lda.fit <- lda(y~age+log(duration),data=dat,subset=train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit,dat[!train,])

lda.class <- lda.pred$class
table(lda.class,y.test)
mean(lda.class == y.test)

# note we can get the total number of classifications by thresholding at 50%
sum(lda.pred$posterior[,1] > 0.5)
sum(lda.pred$class == "no") # or "yes"

## Plot of decision boundary
newdat <- data.frame(expand.grid(seq(18,90,length.out=200),seq(3,3100,length.out=200)))
names(newdat) <- c("age","duration")
new.pred <- predict(lda.fit,newdat)

plot(age~log(duration),data=dat,col="white")
points(log(newdat$duration[new.pred$class=="yes"]),newdat$age[new.pred$class=="yes"],
  pch=3,cex=0.1,col="blue")
points(log(newdat$duration[new.pred$class=="no"]),newdat$age[new.pred$class=="no"],
  pch=3,cex=0.1,col="red")
points(age~log(duration),data=dat,subset=dat$y=="no",col="red",pch=19,cex=0.5)
points(age~log(duration),data=dat,subset=dat$y=="yes",col="blue",pch=19,cex=0.5)

##
## QDA
##

qda.fit <- qda(y~age+log(duration),data=dat,subset=train)
qda.fit

qda.pred <- predict(qda.fit,dat[!train,])

qda.class <- qda.pred$class
table(qda.class,y.test)
mean(qda.class == y.test)

## Plot of decision boundary
new.pred <- predict(qda.fit,newdat)
points(log(newdat$duration[new.pred$class=="yes"]),newdat$age[new.pred$class=="yes"],
  pch=3,cex=0.1,col="grey")

##
## Compare lda/qda fits with ROC curves
##

library(pROC)

r.lda <- roc(y.test~lda.pred$posterior[,2])
r.qda <- roc(y.test~qda.pred$posterior[,2])

plot(r.lda)
plot(r.qda,add=TRUE,col="red")

auc(r.lda)
auc(r.qda)

################################################################################################
## Classifying ionospheric detectors: linear and quadratic discriminant analysis
## (good = some structure in ionosphere, bad = no structure and signal passes through)
## see Sigillito et al. (1989)
################################################################################################

rm(list=ls())

library(MASS)

dat <- read.csv("~/Documents/Classes/Fall2023/5610/data/ionosphere.data",header=FALSE)

set.seed(103888)
n <- dim(dat)[1]
train <- sample(c(FALSE,TRUE),size=n,replace=TRUE)

##
## Need covariates to be approximately normally distributed
##

goods <- dat[,35]=="g"
# (14 23)

plot(dat[,31]~dat[,27],col="white")
points(dat[goods,31]~dat[goods,27],pch=19,col="green")
points(dat[!goods,31]~dat[!goods,27],pch=19,col="red") # good luck

##
## LDA
##

lda.fit <- lda(V35~V27+V31,data=dat,subset=train)
lda.fit
plot(lda.fit)

lda.pred <- predict(lda.fit,dat[!train,])
lda.class <- lda.pred$class

table(lda.class,dat$V35[!train])
mean(lda.class == dat$V35[!train])

##
## QDA
##

qda.fit <- qda(V35~V27+V31,data=dat,subset=train)
qda.fit

qda.pred <- predict(qda.fit,dat[!train,])
qda.class <- qda.pred$class

table(qda.class,dat$V35[!train])
mean(qda.class == dat$V35[!train])

##
## Compare lda/qda fits with ROC curves
##

library(pROC)

r.lda <- roc(dat$V35[!train]~lda.pred$posterior[,2])
r.qda <- roc(dat$V35[!train]~qda.pred$posterior[,2])

plot(r.lda) # troubling!
plot(r.qda,add=TRUE,col="red")

auc(r.lda)
auc(r.qda)












