################################################################################################
## Gaussian process regression
################################################################################################

library(fields)

set.seed(8)
n <- 5
np <- 500
x.pred <- seq(0,10,length.out=np)
these <- sample(1:500,size=5)

y.true <- t(chol(Matern(rdist(x.pred),nu=2.5))) %*% rnorm(np)
x <- x.pred[these]
y <- y.true[these]

plot(x,y,ylim=c(0,2))

##
## GP prediction with perfect observations (y = f(x))
##

Sigma.p <- Matern(rdist(x.pred,x),nu=2.5)
Sigma.obs <- Matern(rdist(x),nu=2.5)
Sigma <- Matern(rdist(x.pred),nu=2.5)

## Predictive mean
y.pred <- Sigma.p %*% solve(Sigma.obs) %*% y 

plot(x,y,ylim=c(0,2))
lines(x.pred,y.pred,col="red")

## Predictive 95% confidence intervals
pred.var <- Sigma - Sigma.p %*% solve(Sigma.obs) %*% t(Sigma.p)
pred.se <- sqrt(diag(pred.var)) # predictive standard errors
pred.se[is.na(pred.se)] <- 0

lines(x.pred,y.pred+1.96*pred.se,lty=2,col="red")
lines(x.pred,y.pred-1.96*pred.se,lty=2,col="red")

lines(x.pred,y.true)
legend(x="topright",legend=c("prediction","truth"),col=c("red","black"),lty=c(1,1))

## Using a different covariance kernel
Sigma.p <- Matern(rdist(x.pred,x),nu=1.5)
Sigma.obs <- Matern(rdist(x),nu=1.5)
Sigma <- Matern(rdist(x.pred),nu=1.5)
y.pred <- Sigma.p %*% solve(Sigma.obs) %*% y
pred.var <- Sigma - Sigma.p %*% solve(Sigma.obs) %*% t(Sigma.p)
pred.se <- sqrt(diag(pred.var)) # predictive standard errors
pred.se[is.na(pred.se)] <- 0
lines(x.pred,y.pred,col="darkblue")
lines(x.pred,y.pred+1.96*pred.se,lty=2,col="darkblue")
lines(x.pred,y.pred-1.96*pred.se,lty=2,col="darkblue")

##
## GP prediction with noisy observations (y = f(x) + eps)
##

y <- y + rnorm(n,mean=0,sd=0.1)

Sigma.p <- Matern(rdist(x.pred,x),nu=2.5)
Sigma.obs <- Matern(rdist(x),nu=2.5) + diag(0.1^2,n)
Sigma <- Matern(rdist(x.pred),nu=2.5)

## Predictive mean
y.pred <- Sigma.p %*% solve(Sigma.obs) %*% y

plot(x,y,ylim=c(0,2))
lines(x.pred,y.pred,col="red")

## Predictive 95% confidence intervals
pred.var <- Sigma - Sigma.p %*% solve(Sigma.obs) %*% t(Sigma.p)
pred.se <- sqrt(diag(pred.var)) # predictive standard errors
pred.se[is.na(pred.se)] <- 0

lines(x.pred,y.pred+1.96*pred.se,lty=2,col="red")
lines(x.pred,y.pred-1.96*pred.se,lty=2,col="red")

lines(x.pred,y.true)
legend(x="topright",legend=c("prediction","truth"),col=c("red","black"),lty=c(1,1))

##
## GPs as smoothers
##

rm(list=ls())

## Name popularity in Colorado data
dat <- read.csv("~/Documents/Classes/STAT5610data/StateNamesCO.csv",header=TRUE)

rate <- dat$Rate[dat$Name == "William" & dat$Gender == "M"]
year <- dat$Year[dat$Name == "William" & dat$Gender == "M"]

plot(rate~year,type="b",xlab="Year",ylab="Popularity (%)",ylim=c(0,0.055))
abline(h=0)

dist.mat <- rdist(year)
Sigma <- Matern(dist.mat/10,nu=2.5)

## sigma^2 too small
fitted <- c(Sigma %*% solve(Sigma + diag(1e-5,length(rate))) %*% c(rate-mean(rate))) + mean(rate)
lines(fitted~year,col="blue",lwd=2)

## sigma^2 too large
fitted <- c(Sigma %*% solve(Sigma + diag(1,length(rate))) %*% c(rate-mean(rate))) + mean(rate)
lines(fitted~year,col="orange",lwd=2)

## Reasonable sigma^2
fitted <- c(Sigma %*% solve(Sigma + diag(0.1,length(rate))) %*% c(rate-mean(rate))) + mean(rate)
lines(fitted~year,lwd=3)

## Choose sigma^2 by cross-validation
set.seed(54)
these <- sample(c(0,1),size=length(rate),replace=TRUE)
rate.out <- rate[these == 1]
rate.in <- rate[these == 0]
year.out <- year[these == 1]
year.in <- year[these == 0]

sigma.grd <- 10^seq(-5,0,length.out=100)
CV <- NULL

for(i in 1:length(sigma.grd)){
  Sigma.p <- Matern(rdist(year.out,year.in)/10,nu=2.5)
  Sigma.obs <- Matern(rdist(year.in)/10,nu=2.5) + diag(sigma.grd[i]^2,length(year.in))
  rate.pred <- Sigma.p %*% solve(Sigma.obs) %*% rate.in
  CV[i] <- mean( (rate.pred - rate.out)^2 )
}

plot(sigma.grd,CV)
sigma.grd[which.min(CV)] # estimate of optimal sigma (SD)
abline(v=sigma.grd[which.min(CV)])

## Final smoothed estimate
plot(rate~year,type="b",xlab="Year",ylab="Popularity (%)",ylim=c(0,0.055))
abline(h=0)
fitted <- c(Sigma %*% solve(Sigma + diag(sigma.grd[which.min(CV)]^2,length(rate))) %*%
  c(rate-mean(rate))) + mean(rate)
lines(fitted~year,lwd=3)
