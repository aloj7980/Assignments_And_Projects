################################################################################################
## Q-Q plots
################################################################################################

##
## Q-Q plot for iid N(0,1) samples
##

set.seed(1)
dat <- rnorm(101,0,1)
# Look at the 95% quantile
sort(dat)
sort(dat)[96]
quantile(dat,probs=0.95)
# if dat is from a N(0,1) distribution, then this quantile *should* match up with
qnorm(0.95)

# Look at the 50% quantile
sort(dat)[51]
quantile(dat,probs=0.5)
# if dat is from a N(0,1) distribution, then this quantile *should* match up with
qnorm(0.5)

## Q-Q plot
qqnorm(dat)
# x-values are at the following quantiles for a N(0,1) distribution
ppoints(101) # probabilities
qnorm(ppoints(101)) # quantiles
rug(qnorm(ppoints(101)))
rug(sort(dat),side=2)
# y-values are at the data points
qqline(dat)

##
## Q-Q plot for iid nonnormal samples
##

rm(list=ls())

set.seed(1)
dat <- rt(1000,df=5)
hist(dat,freq=FALSE)
xseq <- seq(-5,5,length=100)
lines(dnorm(xseq)~xseq)

## Look at the 97.5% quantile
quantile(dat,probs=0.975)
# if dat is from a N(0,1) distribution, then this quantile *should* match up with
qnorm(0.975)

## Q-Q plot
qqnorm(dat)
qqline(dat)

################################################################################################
## Prediction intervals: IMDB data
################################################################################################

rm(list=ls())

# 1000 "most popular" movies from 2006-2016
dat <- read.csv("~/Documents/Classes/Fall2023/5610/data/IMDB-Movie-Data.csv")

names(dat)

str(dat)
summary(dat)

# just some exploratory plots
hist(dat$Rating)
hist(dat$Votes)
hist(log(dat$Votes))

summary(dat$Runtime..Minutes)

# longest movie
dat[which.max(dat$Runtime),]

# highest and lowest ratings
dat[which.max(dat$Rating),]
dat[which.min(dat$Rating),]

# most and fewest votes
dat[which.max(dat$Votes),]
dat[which.min(dat$Votes),]

names(dat)
colnames(dat)[8] <- "Runtime" # in minutes
colnames(dat)[11] <- "Revenue" # in millions

##
## Are "popular" movies getting longer or shorter over time, on average?
##

plot(Runtime~Year,data=dat)

fit <- lm(Runtime~Year,data=dat)

summary(fit) # interpret slope
abline(fit$coef,col="red")

## CI for slope
confint(fit,level=0.95)

## Plot confidence interval for mean
newvals <- 2006:2030 # try for 2006:2150
plot(Runtime~Year,data=dat,xlim=range(newvals))
preds.c <- predict.lm(fit,newdata=data.frame(Year=newvals),interval="confidence")
lines(newvals,preds.c[,1],col="red",lwd=2)
lines(newvals,preds.c[,2],col="green",lwd=2,lty=2)
lines(newvals,preds.c[,3],col="green",lwd=2,lty=2)

## Calculate prediction intervals
preds.p <- predict.lm(fit,newdata=data.frame(Year=newvals),interval="prediction")
lines(newvals,preds.p[,2],col="blue",lwd=2,lty=2)
lines(newvals,preds.p[,3],col="blue",lwd=2,lty=2)

## Forecast for 2025:2030
tail(cbind(newvals,preds.c))

################################################################################################
## Multiple linear regression: College graduation rate
################################################################################################

rm(list=ls())

library(ISLR2)

data(College)
?College

names(College)

College <- subset(College,select=c("Accept","F.Undergrad","Outstate","S.F.Ratio",
  "Expend","Grad.Rate"))

pairs(Grad.Rate~.,data=College,pch=19,cex=0.1)

## Any bad data?
summary(College)
which(College$Grad.Rate > 100)
rownames(College)[96]

College <- College[-96,] # (Leland Stanford graduated from Cazenovia)

#boxplot(College)
#boxplot(scale(College))

##
## First multiple regression model:
## Grad.Rate ~ Outstate + S.F.Ratio
##

y <- College$Grad.Rate
hist(y)
n <- length(y) # number of data points

## Let R do the work for you:
fit <- lm(Grad.Rate~Outstate+S.F.Ratio,data=College) # Note the plus
summary(fit)

## R's version of the covariance matrix
summary(fit)$cov.unscaled
?summary.lm # cov.unscaled is (t(X) %*% X)^{-1}
summary(fit)$cov.unscaled * summary(fit)$sigma^2 # the usual sigma^2*(t(X) %*% X)^{-1}

## Diagnostics
par(mfrow=c(2,2))
plot(fit)
# any outliers?
which(rownames(College) == "Texas Southern University")
College[585,]

which(rownames(College) == "Missouri Southern State College")
College[377,]

# any high leverage points?
sort(hatvalues(fit))
which.max(hatvalues(fit))
College[275,]
summary(College)
hist(College$S.F.Ratio)
abline(v=College$S.F.Ratio[275])

# Compare a few different fits, removing ``outliers'' and high leverage points
# full data
summary(fit)$coef
# no outliers
summary(lm(Grad.Rate~Outstate+S.F.Ratio,data=College[-c(585,377),]))$coef
# no outliers or high leverage points
summary(lm(Grad.Rate~Outstate+S.F.Ratio,data=College[-c(585,377,275),]))$coef

## Predict for CU (in 2019 graduate rate was 69.9%)
# R uses t-quantiles for predictive confidence intervals
# 95% interval for the mean
predict(fit,newdata=data.frame(Outstate=20000,S.F.Ratio=18),interval="confidence")
# 95% prediction interval (uncertainty in a new school at this particular covariate)
predict(fit,newdata=data.frame(Outstate=20000,S.F.Ratio=18),interval="prediction")

##
## Some exploratory multiple regressions
##

fit1 <- lm(Grad.Rate~.,data=College)
summary(fit1)
fit2 <- update(fit1,formula=~.-Expend-S.F.Ratio)
summary(fit2)
# compare models using information criteria
AIC(fit1,fit2)
BIC(fit1,fit2)
# remember, lower is better
# can also compare more models
fit3 <- update(fit2,formula=~.-F.Undergrad)
summary(fit3)
BIC(fit1,fit2,fit3)

# Leverage
par(mfrow=c(2,2))
plot(fit2)
sort(hatvalues(fit2))
which(rownames(College) == "Rutgers at New Brunswick")
College[483,]
summary(College)

hist(College$Accept)
abline(v=College$Accept[483])
# interpret parameters

# Cook's distance
cbind(sort(cooks.distance(fit2)))
# not really any single points that unusually affect model fit
# in particular, Rutgers could potentially influence the fit with its high leverage
# but doesn't really seem to, according to Cook's distance
# ... could just refit the model without it to see how the fit changes

summary(fit2)
cov2cor(summary(fit2)$cov.unscaled)
# note relationship between the estimate of beta for Accept and F.Undergrad!

